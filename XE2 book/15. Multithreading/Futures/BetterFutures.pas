unit BetterFutures;
{
  See BetterFutures.txt for info.
}
interface

uses
  System.SysUtils, System.Classes, System.SyncObjs;

type
  TFutureTaskToken = type Pointer; //Opaque pointer type.

  TFutureTasks = record            //Defines and implements a thread pool for the futures.
  strict private type
    PNode = ^TNode;                //Use a simple linked list.
    TNode = record
      NextNode: PNode;
      Task: TProc;
    end;
  strict private
    class var FHeadNode, FTailNode: PNode;
    class var FTerminated: Boolean;
    class var FThreadsLeft: TCountdownEvent;
    class var FQueueLock: TObject;
    class constructor Create;
    class destructor Destroy;
    class function WaitFor(out ATask: TProc): Boolean; static;
  public
    class var TimeoutForCleaningUp: LongWord;
    class function Add(const ATask: TProc): TFutureTaskToken; static;
    class function Cancel(ACancellationToken: TFutureTaskToken): Boolean; static;
    class procedure CancelAll; static;
    class procedure Terminate; static;
  end;

  TFuture<T> = record
  strict private type
    TFutureImpl = class(TInterfacedObject, TFunc<T>) //An anonymous method is really an interface
    strict private                                   //with a single method called Invoke.
      FCompletedEvent: TLightweightEvent;
      FFatalException: TObject;
      FInvokeTimeout: LongWord;
      FResult: T;
    public
      constructor Create(const ADelegate: TFunc<T>; out ACancellationToken: TFutureTaskToken;
        AInvokeTimeout: LongWord);
      destructor Destroy; override;
      function Invoke: T;
    end;
  public
    class function Create(const ADelegate: TFunc<T>; AInvokeTimeout: LongWord = INFINITE): TFunc<T>; overload; static; inline;
    class function Create(const ADelegate: TFunc<T>; out ACancellationToken: TFutureTaskToken;
      AInvokeTimeout: LongWord = INFINITE): TFunc<T>; overload; static; inline;
  end;

resourcestring
  SFuturesTaskPoolTerminated = 'Futures task pool has been terminated';
  SFutureInvokeTimeout = 'Invoking a future timed out';

implementation

{ TFutureTasks }

class constructor TFutureTasks.Create;
var
  I: Integer;
begin
  FQueueLock := TObject.Create;
  FThreadsLeft := TCountdownEvent.Create(1);
  for I := 1 to CPUCount * 2 do      //Create twice as many worker threads as there are CPU cores.
    TThread.CreateAnonymousThread(
      procedure
      var
        Task: TProc;
      begin
        FThreadsLeft.AddCount;       //Increment the active threads counter.
        try
          while WaitFor(Task) do     //Loop waiting for posted tasks to perform.
            Task;                    //Invoke the task.
        finally
          FThreadsLeft.Signal;       //Decrement the active threads counter - use a try/finally to
        end;                         //make sure this will happen.
      end).Start;                    //Get the thread going immediately.
  TimeoutForCleaningUp := 5000;
end;

class destructor TFutureTasks.Destroy;
begin
  Terminate;                         //Notify the worker threads to stop waiting.
  FThreadsLeft.Signal;               //Release our 1.
  if FThreadsLeft.WaitFor(TimeoutForCleaningUp) <> wrSignaled then Exit;
  FQueueLock.Free;                   //Don't free anything if a worker thread has apparently hung so
  FThreadsLeft.Free;                 //as not to cause an access violation - the OS will ultimately
end;                                 //clean up anyhow.

class function TFutureTasks.Add(const ATask: TProc): TFutureTaskToken;
var
  Node: PNode;
begin
  Node := AllocMem(SizeOf(TNode));
  try
    Node.Task := ATask;
    TMonitor.Enter(FQueueLock);
    try
      if FTerminated then raise EInvalidOperation.CreateRes(@SFuturesTaskPoolTerminated);
      if FHeadNode = nil then
        FHeadNode := Node
      else
        FTailNode.NextNode := Node;
      FTailNode := Node;
      TMonitor.Pulse(FQueueLock);    //Pulse to let 1 waiting worker thread loose.
    finally
      TMonitor.Exit(FQueueLock);
    end;
  except
    Dispose(Node);
    raise;
  end;
  Result := TFutureTaskToken(Node);
end;

class function TFutureTasks.Cancel(ACancellationToken: TFutureTaskToken): Boolean;
var
  Previous, Current: PNode;
begin
  Previous := nil; //keep compiler happy
  TMonitor.Enter(FQueueLock);
  try
    Current := FHeadNode;
    while Current <> nil do
      if TFutureTaskToken(Current) = ACancellationToken then
      begin
        if Current = FHeadNode then
          FHeadNode := Current.NextNode
        else
          Previous.NextNode := Current.NextNode;
        Dispose(Current);
        Exit(True);
      end
      else
      begin
        Previous := Current;
        Current := Current.NextNode;
      end;
  finally
    TMonitor.Exit(FQueueLock);
  end;
  Result := False;
end;

class procedure TFutureTasks.CancelAll;
var
  Next: PNode;
begin
  TMonitor.Enter(FQueueLock);
  try
    while FHeadNode <> nil do
    begin
      Next := FHeadNode.NextNode;
      Dispose(FHeadNode);
      FHeadNode := Next;
    end;
    FTailNode := nil;
  finally
    TMonitor.Exit(FQueueLock);
  end;
end;

class procedure TFutureTasks.Terminate;
begin
  FTerminated := True;
  TMonitor.PulseAll(FQueueLock);
  CancelAll;
end;

class function TFutureTasks.WaitFor(out ATask: TProc): Boolean;
var
  OldNode: PNode;
begin
  TMonitor.Enter(FQueueLock);
  try
    while FHeadNode = nil do
    begin
      TMonitor.Wait(FQueueLock, INFINITE); //Release the critical section and await pulsing.
      if FTerminated then Exit(False);
    end;
    OldNode := FHeadNode;                  //Move the top task off the task queue.
    FHeadNode := OldNode.NextNode;
  finally
    TMonitor.Exit(FQueueLock);
  end;
  ATask := OldNode.Task;
  Dispose(OldNode);
  Result := True;
end;

{ TFuture<T>.TFutureImpl }

constructor TFuture<T>.TFutureImpl.Create(const ADelegate: TFunc<T>;
  out ACancellationToken: TFutureTaskToken; AInvokeTimeout: LongWord);
begin
  inherited Create;
  FCompletedEvent := TLightweightEvent.Create;
  FInvokeTimeout := AInvokeTimeout;
  { An annoying scenario to cover is when the calling code never invokes the future, and instead
    just lets it fall out of scope: in such a case, the task may still be on the task pool ready to
    be plucked off by one of the worker threads. }
  ACancellationToken := TFutureTasks.Add(
    procedure
    var
      DoFree: Boolean;
    begin
      TMonitor.Enter(FCompletedEvent);                //While the SetEvent method is threadsafe,
      try                                             //the event object's destruction is not.
        DoFree := FCompletedEvent.IsSet;              //If the event is already set, then the parent
        if not DoFree then                            //object has already been freed (see Destroy).
        begin
          try                                         //Trap any exception so as to re-raise it in
            FResult := ADelegate;                     //the context of the calling thread (see
          except                                      //Invoke method below).
            FFatalException := AcquireExceptionObject;
          end;
          FCompletedEvent.SetEvent;
        end;
      finally
        TMonitor.Exit(FCompletedEvent);
      end;
      if DoFree then FCompletedEvent.Free;
    end)
end;

destructor TFuture<T>.TFutureImpl.Destroy;
var
  DoFree: Boolean;
begin
  TMonitor.Enter(FCompletedEvent);                    //Get a lock on the event object; if it is not
  try                                                 //yet set, then our task must still be on the
    DoFree := FCompletedEvent.IsSet;                  //task pool waiting to be executed.
    if not DoFree then FCompletedEvent.SetEvent;
  finally
    TMonitor.Exit(FCompletedEvent);
  end;
  if DoFree then FCompletedEvent.Free;
  FFatalException.Free;
  inherited;
end;

function TFuture<T>.TFutureImpl.Invoke: T;
var
  ExceptObject: TObject;
begin
  if FCompletedEvent.WaitFor(FInvokeTimeout) = wrTimeout then    //Wait for the task to finish.
    raise ESyncObjectException.CreateRes(@SFutureInvokeTimeout);
  if FFatalException = nil then Exit(FResult);                   //If no exception, then great.
  ExceptObject := FFatalException;                               //Otherwise, we need to copy then
  if ExceptObject is Exception then                              //re-raise it.
    FFatalException := ExceptClass(ExceptObject.ClassType).Create(Exception(ExceptObject).Message)
  else                                                           //The exception is copied in case
    FFatalException := ExceptObject.ClassType.Create;            //we get called again, and so, need
  raise ExceptObject;                                            //to re-raise the exception again.
end;

{ TFuture<T> }

class function TFuture<T>.Create(const ADelegate: TFunc<T>; AInvokeTimeout: LongWord): TFunc<T>;
var
  AToken: TFutureTaskToken;
begin
  Result := TFutureImpl.Create(ADelegate, AToken, AInvokeTimeout);
end;

class function TFuture<T>.Create(const ADelegate: TFunc<T>;
  out ACancellationToken: TFutureTaskToken; AInvokeTimeout: LongWord): TFunc<T>;
begin
  Result := TFutureImpl.Create(ADelegate, ACancellationToken, AInvokeTimeout);
end;

end.

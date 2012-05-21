program CountdownEventExample;
{
  For a number of reasons, the WaitFor method of TThread has its flaws. Using
  TCountdownEvent makes for a simple alternative. The only thing you need to be
  a bit careful of is ensuring Signal will be called by the worker thread object
  in an appropriate place. If FreeOnTerminate is False, or you are using an
  anonymous thread, this should be in the 'finally' block of a general 'try'
  statement in the Execute or thread method:

  procedure TMyThread.Execute;
  begin
    FCountdownEvent.AddCount;
    try
      //actual work here...
    finally
      FCountdownEvent.Signal;
    end;
  end;

  Note that if OnTerminate has been assigned, this will mean the countdown event
  will be signalled *before* OnTerminate is called.

  When FreeOnTerminate is True and an explicit TThread descendant is used, an
  alternative is to call Signal in Destroy, as shown in the example below. While
  OnTerminate isn't assigned here, if it were, the handler would be called prior
  to the event being signalled.
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.Classes, System.SyncObjs;

var
  ConsoleCS: TCriticalSection;

procedure WriteLineToConsole(const S: string; const Args: array of const);
begin
  ConsoleCS.Enter;
  try
    WriteLn(Format(S, Args));
  finally
    ConsoleCS.Leave;
  end;
end;

type
  TWorkerThread = class(TThread)
  strict private
    FCountdownEvent: TCountdownEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(ACountdownEvent: TCountdownEvent);
    destructor Destroy; override;
  end;

constructor TWorkerThread.Create(ACountdownEvent: TCountdownEvent);
begin
  inherited Create(True);
  ACountdownEvent.AddCount;           //better safe than sorry: only assign the
  FCountdownEvent := ACountdownEvent; //field once AddCount has actually succeeded
  FreeOnTerminate := True;
end;

destructor TWorkerThread.Destroy;
begin
  if FCountdownEvent <> nil then FCountdownEvent.Signal;
  inherited;
end;

procedure TWorkerThread.Execute;
begin
  WriteLineToConsole('Started %d', [TThread.CurrentThread.ThreadID]);
  TThread.SpinWait(Random(MaxLongInt)); //simulate doing work...
  WriteLineToConsole('Finishing %d', [TThread.CurrentThread.ThreadID]);
end;

var
  CountdownEvent: TCountdownEvent;
  I, WorkerCount: Integer;
  S: string;
begin
  Randomize;
  //get the number of worker threads to create...
  repeat
    Write('Type number of worker threads to create and press ENTER: ');
    ReadLn(S);
    if (S = '') then Exit;
  until TryStrToInt(S, WorkerCount);
  if WorkerCount < 1 then Exit;

  CountdownEvent := nil;
  ConsoleCS := TCriticalSection.Create;
  try
    //a countdown event must have an initial count of at least 1
    CountdownEvent := TCountdownEvent.Create(1);
    for I := 1 to WorkerCount do
      TWorkerThread.Create(CountdownEvent).Start;
    //release our 1 and wait for the worker threads to finish...
    WriteLineToConsole('Waiting for countdown event object to be signalled...', []);
    CountdownEvent.Signal;
    CountdownEvent.WaitFor;
    WriteLineToConsole('Done!', []);
  finally
    CountdownEvent.Free;
    ConsoleCS.Free;
  end;
  Write('Press ENTER to exit...');
  ReadLn;
end.

program SemaphoreBasics;
{
  Simple semaphore demo. In it, two semaphores are used: one to throtle the
  worker threads so that only three do any work at any one time, and a second
  (in league with TInterlocked) to allow the main thread to wait on the worker
  threads until have all finished.
}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Classes, System.SyncObjs;

var
  ConsoleCS: TCriticalSection;

procedure WriteLineToConsole(const S: string);
begin
  ConsoleCS.Enter;
  try
    WriteLn(S);
  finally
    ConsoleCS.Leave;
  end;
end;

var
  I: Integer;
  NumWorkersLeft: Integer;
  ThrottleSemaphore: TLightweightSemaphore = nil;
  WorkersSemaphore: TSemaphore = nil;
begin
  Randomize;
  ConsoleCS := TCriticalSection.Create;
  try
    ThrottleSemaphore := TLightweightSemaphore.Create(3); //max 3 slots
    WorkersSemaphore := TSemaphore.Create;                //max 1 slot
    WorkersSemaphore.Acquire;                             //grab that slot
    NumWorkersLeft := 10;
    WriteLineToConsole('Creating worker threads...');
    for I := 1 to NumWorkersLeft do
      TThread.CreateAnonymousThread(
        procedure
        var
          IDStr: string;
          SpinIterations: Integer;
        begin
          try
            IDStr := '  Thread ' + IntToStr(TThread.CurrentThread.ThreadID);
            WriteLineToConsole(IDStr + ' wants to do some work...');
            ThrottleSemaphore.Acquire;
            try
              SpinIterations := $70000000 + Random($FFFFFFF);
              WriteLineToConsole(IDStr + ' is now working, and will ' +
                'spin for ' + IntToStr(SpinIterations) + ' iterations...');
              TThread.SpinWait(SpinIterations);
              WriteLineToConsole(IDStr + ' has finished');
            finally
              ThrottleSemaphore.Release;
            end;
          finally
            if TInterlocked.Decrement(NumWorkersLeft) = 0 then
              WorkersSemaphore.Release; //releases the main thread, which is blocking on the semaphore
          end;
        end).Start;
    WriteLineToConsole('Waiting for workers to finish...');
    WorkersSemaphore.Acquire;
  finally
    WorkersSemaphore.Free;
    ThrottleSemaphore.Free;
    ConsoleCS.Free;
  end;
  Write('All done!');
  ReadLn;
end.

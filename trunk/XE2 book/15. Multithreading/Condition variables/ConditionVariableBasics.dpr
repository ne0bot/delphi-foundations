program ConditionVariableBasics;
{
  Simple example of condition variable usage. Here, we use the mutex version. In terms of interface,
  the critical section version is essentially identical. However, under the hood, where
  TConditionVariableMutex maps to a native API primitive on OS X, its implementation is backfilled
  for Windows. Conversely, where TConditionVariableCS maps to a native API primitive on newer
  versions of Windows, it is backfilled on other operating systems.
}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Classes, System.SyncObjs;

var
  Lock: TMutex;
  CondVar: TConditionVariableMutex;
  I: Integer;
begin
  Lock := TMutex.Create;
  CondVar := TConditionVariableMutex.Create;
  try
    //spin up a couple of worker threads
    for I := 1 to 2 do
      TThread.CreateAnonymousThread(
        procedure
        begin
          Lock.Acquire;
          try
            CondVar.WaitFor(Lock);
            WriteLn('Thread ' + IntToStr(TThread.CurrentThread.ThreadID) +
              ' got signalled and will now keep the look for 2 seconds...');
            Sleep(2000);
            WriteLn('... about to release the lock');
          finally
            Lock.Release;
          end;
        end).Start;
      //dawdle for a bit then signal to all waiting threads
      WriteLn('The condition variable will be signalled in 1 second...');
      Sleep(1000);
      CondVar.ReleaseAll;
      ReadLn;
  finally
    CondVar.Free;
    Lock.Free;
  end;
end.

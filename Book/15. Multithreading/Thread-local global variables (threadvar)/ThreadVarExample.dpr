program ThreadVarExample;
{
  Demonstrates the difference using 'threadvar' over 'var' makes to a global variable:
  each thread gets its own copy.
}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows;

var
  SomeNumNormal: Integer;

threadvar
  SomeNumThread: Integer;

function ThreadFunc(Data: Pointer): Integer;
begin
  Inc(SomeNumNormal, 100);
  Inc(SomeNumThread, 100);
  WriteLn('Thread ', GetCurrentThreadID, ':');
  WriteLn('SomeNumNormal = ', SomeNumNormal);
  WriteLn('SomeNumThread = ', SomeNumThread);
  WriteLn('');
  Result := 0;
end;

var
  Handles: array[1..2] of THandle;
  IDs: array[1..2] of TThreadID;
  I: Integer;
begin
  SomeNumNormal := 42;
  SomeNumThread := 42;
  for I := Low(Handles) to High(Handles) do
  begin
    Handles[I] := BeginThread(nil, 0, ThreadFunc, nil, 0, IDs[I]);
    Sleep(100);
  end;
  { wait for the threads to terminate }
  WaitForMultipleObjects(Length(Handles), @Handles, True, INFINITE);
  { threads creating via BeginThread must be explicitly freed using CloseHandle
    on Windows (pthread_detatch on OS X) }
  for I := Low(Handles) to High(Handles) do
    CloseHandle(Handles[I]);
  Write('Press ENTER to exit...');
  ReadLn;
end.

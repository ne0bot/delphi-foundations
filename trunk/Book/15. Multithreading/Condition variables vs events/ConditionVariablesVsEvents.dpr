program ConditionVariablesVsEvents;
{
  Simple demo of how signaling a condition variable that does not have any waiters at the time the
  signal is made will cause the signal to be 'lost', in contrast to what happens with an event.
}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Classes, System.SyncObjs;

procedure TestEvent;
var
  Event: TEvent; //or TLightweightEvent - it doesn't matter for the purposes of this demo
begin
  Event := TEvent.Create;
  try
    WriteLn('Signalling the event...');
    Event.SetEvent;
    WriteLn('Creating a background thread to wait on the event...');
    TThread.CreateAnonymousThread(
      procedure
      begin
        case Event.WaitFor(5000) of
          wrTimeout: WriteLn('  Timed out!');
          wrSignaled: WriteLn('  Got the signal');
        end;
      end).Start;
    Sleep(100);
  finally
    Event.Free;
  end;
end;

procedure TestConditionVariable;
var
  Lock: TCriticalSection;
  CondVar: TConditionVariableCS;
begin
  Lock := TCriticalSection.Create;
  CondVar := TConditionVariableCS.Create;
  try
    WriteLn('Signalling the condition variable...');
    CondVar.Release;
    WriteLn('Creating a background thread to wait on the condition variable...');
    TThread.CreateAnonymousThread(
      procedure
      begin
        Lock.Acquire;
        try
          case CondVar.WaitFor(Lock, 5000) of
            wrTimeout: WriteLn('  Timed out!');
            wrSignaled: WriteLn('  Got the signal');
          end;
        finally
          Lock.Release;
        end;
      end).Start;
    ReadLn;
  finally
    CondVar.Free;
    Lock.Free;
  end;
end;

begin
  TestEvent;
  WriteLn;
  TestConditionVariable;
end.

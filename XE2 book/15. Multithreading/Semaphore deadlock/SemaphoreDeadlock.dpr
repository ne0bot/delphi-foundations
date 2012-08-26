program SemaphoreDeadlock;
{
  Simple demo of how a so-called 'binary semaphore' (i.e., a semaphore with only
  one 'lock' available) is not the same as a critical section object or monitor,
  and can easily lead to deadlocks as a result.
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.SyncObjs;

var
  CriticalSection: TCriticalSection;
  Lock: TObject;
  Semaphore: TSynchroObject;
  S: string;
begin
  { Critical section }
  CriticalSection := TCriticalSection.Create;
  try
    CriticalSection.Acquire;
    try
      WriteLn('Taken first lock...');
      CriticalSection.Acquire;
      try
        WriteLn('We''ve made it!');
      finally
        CriticalSection.Release;
      end;
    finally
      CriticalSection.Release;
    end;
  finally
    CriticalSection.Free;
  end;
  Writeln('TCriticalSection version ran fine', sLineBreak);
  { Monitor }
  Lock := TObject.Create;
  try
    TMonitor.Enter(Lock);
    try
      WriteLn('Taken first lock...');
      TMonitor.Enter(Lock);
      try
        WriteLn('We''ve made it!');
      finally
        TMonitor.Exit(Lock);
      end;
    finally
      TMonitor.Exit(Lock);
    end;
  finally
    Lock.Free;
  end;
  WriteLn('TMonitor version ran fine', sLineBreak);
  { Semaphore - user chooses which semaphore class to use }
  Semaphore := nil; //avoid compiler warning
  repeat
    Write('Test the ''lightweight'' semaphore class? [Y/N]');
    ReadLn(S);
    if S = '' then Continue;
    case S[1] of
      'Y', 'y': Semaphore := TLightweightSemaphore.Create(1);
      'N', 'n': Semaphore := TSemaphore.Create(nil, 1, MaxInt, '');
    else
      Continue;
    end;
  until True;
  try
    WriteLn(sLineBreak + 'Now testing with ', Semaphore.ClassName, ' - this should hang!' + sLineBreak);
    Semaphore.Acquire;
    try
      WriteLn('Taken first lock...');
      Semaphore.Acquire;
      try
        WriteLn('This will never be reached!');
      finally
        Semaphore.Release;
      end;
    finally
      Semaphore.Release;
    end;
  finally
    Semaphore.Free;
  end;
  Write('Moral of the story: semaphores with only one ''slot'' ' +
        'are not the same thing as critical section objects');
end.

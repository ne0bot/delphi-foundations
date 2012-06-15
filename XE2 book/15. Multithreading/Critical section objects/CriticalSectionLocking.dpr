program CriticalSectionLocking;
{
  (Very) simple locking demo: WriteLn is not threadsafe, so we create a wrapper
  that uses a critical section to serialise access one thread at a time. Note that
  this solution is cooperative in form: if a thread calls WriteLn directly, then
  the original problem will return.
}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.Classes, System.SyncObjs;

type
  TConsole = record
  strict private
    class var FCriticalSection: TCriticalSection;
    class constructor Create;
    class destructor Destroy;
  public
    class procedure WriteLine(const S: string); static;
  end;

class constructor TConsole.Create;
begin
  FCriticalSection := TCriticalSection.Create;
end;

class destructor TConsole.Destroy;
begin
  FCriticalSection.Free;
end;

class procedure TConsole.WriteLine(const S: string);
begin
  FCriticalSection.Enter;
  try
    WriteLn(S);
  finally
    FCriticalSection.Leave;
  end;
end;

var
  SecondaryThread: TThread;
begin
  SecondaryThread := TThread.CreateAnonymousThread(
    procedure
    begin
      TConsole.WriteLine('Hello from the secondary thread');
    end);
  SecondaryThread.Start;
  TConsole.WriteLine('Hello from the main thread');
  ReadLn;
end.

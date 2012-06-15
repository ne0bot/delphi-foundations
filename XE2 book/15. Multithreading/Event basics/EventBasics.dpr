program EventBasics;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Classes, System.SyncObjs;

var
  Event: TEvent;
begin
  //create an auto-reset event whose initial state is unset
  Event := TEvent.Create(nil, False, False, '');
  try
    TThread.CreateAnonymousThread(
      procedure
      begin
        WriteLn('Background thread will now wait on the event to be set...');
        if Event.WaitFor(5000) = wrSignaled then //5 secs timeout
        begin
          WriteLn('Well that took a long time!');
          Event.SetEvent;
        end
        else
          WriteLn('Urgh, something went wrong...');
      end).Start;
    Sleep(4000);
    Event.SetEvent; //let the waiting thread go
    Event.WaitFor;  //now wait on the event ourselves
  finally
    Event.Free;
  end;
  Write('All done now');
  ReadLn;
end.

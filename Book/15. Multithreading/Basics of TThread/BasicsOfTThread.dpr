program BasicsOfTThread;
{
  Ultra-simple demo of TThread. Be warned that synchronisation of WriteLn is
  only avoided because of the timings used in the Sleep calls.
}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes;

type
  TExampleThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TExampleThread.Execute;
begin
  WriteLn('   The secondary thread (which has an ID of ', ThreadID, ') says hello');
  Sleep(1000);
  Writeln('   The secondary thread says goodbye');
end;

var
  Thread: TExampleThread;
begin
  WriteLn('At the level of operating system, the main thread has an ID no. of ',
    TThread.CurrentThread.ThreadID);
  Thread := TExampleThread.Create(True);
  Thread.FreeOnTerminate := True;
  WriteLn('Teeing up secondary thread...');
  Thread.Start;
  Sleep(500);
  Writeln('It has probably said ''hi'' by now...');
  Sleep(2000);
  WriteLn('And it''s goodbye from the main thread too (press ENTER to exit)');
  ReadLn;
end.

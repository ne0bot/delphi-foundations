program NSAlertExample;

uses
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  CCR.NSAlertHelper in 'CCR.NSAlertHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

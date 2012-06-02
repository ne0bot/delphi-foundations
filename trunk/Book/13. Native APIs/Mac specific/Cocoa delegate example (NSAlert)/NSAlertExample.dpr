program NSAlertExample;

uses
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmNSAlert},
  CCR.NSAlertHelper in 'CCR.NSAlertHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmNSAlert, frmNSAlert);
  Application.Run;
end.

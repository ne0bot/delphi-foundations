program NSWindowTipsAndTricks;

uses
  FMX.Forms,
  NSWindowTipsForm in 'NSWindowTipsForm.pas' {frmNSWindowTips};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmNSWindowTips, frmNSWindowTips);
  Application.Run;
end.

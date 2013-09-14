program MacPrefsDemo;

uses
  FMX.Forms,
  CCR.Apple.PrefsIniFile in '..\..\CCR.Apple.PrefsIniFile.pas',
  MacPrefsDemoForm in 'MacPrefsDemoForm.pas' {frmMacPrefsDemo},
  ShellUtils in 'ShellUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMacPrefsDemo, frmMacPrefsDemo);
  Application.Run;
end.

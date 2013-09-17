program MacPrefsDemo;

uses
  FMX.Forms,
  MacPrefsDemoForm in 'MacPrefsDemoForm.pas' {frmMacPrefsDemo},
  ShellUtils in 'ShellUtils.pas',
  CCR.PrefsIniFile.Apple in '..\..\CCR.PrefsIniFile.Apple.pas',
  CCR.PrefsIniFile in '..\..\CCR.PrefsIniFile.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMacPrefsDemo, frmMacPrefsDemo);
  Application.Run;
end.

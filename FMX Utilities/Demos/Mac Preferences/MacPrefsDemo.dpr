program MacPrefsDemo;

uses
  FMX.Forms,
  CCR.MacPrefsIniFile in '..\..\CCR.MacPrefsIniFile.pas',
  MacPrefsDemoForm in 'MacPrefsDemoForm.pas' {frmMacPrefsDemo},
  ShellUtils in 'ShellUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMacPrefsDemo, frmMacPrefsDemo);
  Application.Run;
end.

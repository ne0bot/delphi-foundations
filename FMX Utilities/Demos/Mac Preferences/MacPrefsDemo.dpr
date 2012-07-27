program MacPrefsDemo;

uses
  FMX.Forms,
  CCR.FMXNativeDlgs in '..\..\CCR.FMXNativeDlgs.pas',
  CCR.MacPrefsIniFile in '..\..\CCR.MacPrefsIniFile.pas',
  MacPrefsDemoForm in 'MacPrefsDemoForm.pas' {frmMacPrefsDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMacPrefsDemo, frmMacPrefsDemo);
  Application.Run;
end.

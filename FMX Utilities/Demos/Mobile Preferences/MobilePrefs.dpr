program MobilePrefs;

uses
  System.StartUpCopy,
  FMX.Forms,
  MobilePrefsForm in 'MobilePrefsForm.pas' {frmMobilePrefs},
  CCR.PrefsIniFile.Apple in '..\..\CCR.PrefsIniFile.Apple.pas',
  CCR.PrefsIniFile.Android in '..\..\CCR.PrefsIniFile.Android.pas',
  CCR.PrefsIniFile in '..\..\CCR.PrefsIniFile.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMobilePrefs, frmMobilePrefs);
  Application.Run;
end.

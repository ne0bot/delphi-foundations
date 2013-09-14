program MobilePrefs;

uses
  System.StartUpCopy,
  FMX.Forms,
  MobilePrefsForm in 'MobilePrefsForm.pas' {frmMobilePrefs},
  CCR.Apple.PrefsIniFile in '..\..\CCR.Apple.PrefsIniFile.pas',
  CCR.Android.PrefsIniFile in '..\..\CCR.Android.PrefsIniFile.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMobilePrefs, frmMobilePrefs);
  Application.Run;
end.

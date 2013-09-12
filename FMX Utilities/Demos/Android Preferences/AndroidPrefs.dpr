program AndroidPrefs;

uses
  System.StartUpCopy,
  FMX.Forms,
  AndroidPrefsForm in 'AndroidPrefsForm.pas' {Form1},
  CCR.Android.PrefsIniFile in '..\..\CCR.Android.PrefsIniFile.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

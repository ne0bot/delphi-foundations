program AndroidPrefs;

uses
  System.StartUpCopy,
  FMX.Forms,
  AndroidPrefsForm in 'AndroidPrefsForm.pas' {Form1},
  CCR.PrefsIniFile.Android in '..\..\CCR.PrefsIniFile.Android.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

program MacPrefsTest;

uses
  FMX.Forms,
  TestForm in 'TestForm.pas' {Form1},
  CCR.MacPrefsIniFile in 'CCR.MacPrefsIniFile.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

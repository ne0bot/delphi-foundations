program CompListTextFMX;

uses
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmFMX},
  CCR.Generics.CompList in '..\CCR.Generics.CompList.pas',
  CCR.FMXUtils in '..\..\..\CCR.FMXUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmFMX, frmFMX);
  Application.Run;
end.

program TEncodingUsageVCL;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmEncodingsTest},
  CCR.Encodings in '..\CCR.Encodings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmEncodingsTest, frmEncodingsTest);
  Application.Run;
end.

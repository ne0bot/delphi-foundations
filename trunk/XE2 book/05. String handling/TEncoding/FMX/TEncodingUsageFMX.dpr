program TEncodingUsageFMX;

uses
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmEncodingsTest},
  CCR.Encodings in '..\CCR.Encodings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmEncodingsTest, frmEncodingsTest);
  Application.Run;
end.

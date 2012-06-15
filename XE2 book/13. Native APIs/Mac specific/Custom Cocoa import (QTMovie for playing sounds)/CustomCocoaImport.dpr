program CustomCocoaImport;

uses
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmQTMovie};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmQTMovie, frmQTMovie);
  Application.Run;
end.

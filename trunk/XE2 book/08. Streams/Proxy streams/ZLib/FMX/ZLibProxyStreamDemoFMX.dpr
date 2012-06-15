program ZLibProxyStreamDemoFMX;

uses
  FMX.Forms,
  XMLForm in 'XMLForm.pas' {frmXML};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmXML, frmXML);
  Application.Run;
end.

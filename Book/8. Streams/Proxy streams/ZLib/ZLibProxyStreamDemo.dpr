program ZLibProxyStreamDemo;

uses
  Vcl.Forms,
  XMLForm in 'XMLForm.pas' {frmXML};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmXML, frmXML);
  Application.Run;
end.

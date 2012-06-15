program XPathDemo;

uses
  FMX.Forms,
  XPathForm in 'XPathForm.pas' {frmXPath};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmXPath, frmXPath);
  Application.Run;
end.

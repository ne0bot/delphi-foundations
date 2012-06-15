program XMLBrowserWithMSXMLDirectly;

uses
  Vcl.Forms,
  MSXMLForm in 'MSXMLForm.pas' {frmMSXML};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMSXML, frmMSXML);
  Application.Run;
end.

program XMLBrowserWithIXMLDocument;

uses
  FMX.Forms,
  XMLForm in 'XMLForm.pas' {frmWithIXMLDocument},
  FMXTreeViewUtils in '..\FMXTreeViewUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmWithIXMLDocument, frmWithIXMLDocument);
  Application.Run;
end.

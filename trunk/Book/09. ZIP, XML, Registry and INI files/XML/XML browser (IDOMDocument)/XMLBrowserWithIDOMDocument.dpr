program XMLBrowserWithIDOMDocument;

uses
  FMX.Forms,
  DOMForm in 'DOMForm.pas' {frmWithIDOMDocument},
  FMXTreeViewUtils in '..\FMXTreeViewUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmWithIDOMDocument, frmWithIDOMDocument);
  Application.Run;
end.

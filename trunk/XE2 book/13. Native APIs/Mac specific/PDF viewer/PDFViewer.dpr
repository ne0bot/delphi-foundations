program PDFViewer;

uses
  FMX.Forms,
  PDFViewerForm in 'PDFViewerForm.pas' {frmPDFViewer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmPDFViewer, frmPDFViewer);
  Application.Run;
end.

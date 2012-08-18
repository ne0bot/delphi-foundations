program MacPDFWriterDemo;

uses
  FMX.Forms,
  CCR.FMXNativeDlgs in '..\..\CCR.FMXNativeDlgs.pas',
  CCR.MacPDFWriter in '..\..\CCR.MacPDFWriter.pas',
  PDFWriterForm in 'PDFWriterForm.pas' {frmPDFWriter};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmPDFWriter, frmPDFWriter);
  Application.Run;
end.

program ZipClassMethodsDemo;

uses
  Vcl.Forms,
  EnforceMaxZipFileCompression in 'EnforceMaxZipFileCompression.pas',
  ZipForm in 'ZipForm.pas' {frmZipClassMethods};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmZipClassMethods, frmZipClassMethods);
  Application.Run;
end.

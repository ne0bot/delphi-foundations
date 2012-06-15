program ZipFileBrowser;

uses
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmZipFileBrowser},
  FileUtils in 'FileUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmZipFileBrowser, frmZipFileBrowser);
  Application.Run;
end.

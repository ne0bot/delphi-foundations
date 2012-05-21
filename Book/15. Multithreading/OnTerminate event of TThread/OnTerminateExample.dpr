program OnTerminateExample;

uses
  FMX.Forms,
  DownloadForm in 'DownloadForm.pas' {frmDownloadTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDownloadTest, frmDownloadTest);
  Application.Run;
end.

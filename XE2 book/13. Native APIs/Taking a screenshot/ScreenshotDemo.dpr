program ScreenshotDemo;

uses
  FMX.Forms,
  ScreenshotForm in 'ScreenshotForm.pas' {frmScreenshot};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmScreenshot, frmScreenshot);
  Application.Run;
end.

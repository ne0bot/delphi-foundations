program TextFileViewer;

uses
  FMX.Forms,
  TextFileViewerForm in 'TextFileViewerForm.pas' {frmMain},
  CustomNSApplicationDelegate in 'CustomNSApplicationDelegate.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

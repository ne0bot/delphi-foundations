program ClipboardDemo;

uses
  FMX.Forms,
  ClipboardDemoForm in 'ClipboardDemoForm.pas' {frmClipboardDemo},
  CCR.FMXClipboard.Mac in '..\..\CCR.FMXClipboard.Mac.pas',
  CCR.FMXClipboard in '..\..\CCR.FMXClipboard.pas',
  CCR.FMXClipboard.Win in '..\..\CCR.FMXClipboard.Win.pas',
  CCR.FMXNativeDlgs in '..\..\CCR.FMXNativeDlgs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmClipboardDemo, frmClipboardDemo);
  Application.Run;
end.

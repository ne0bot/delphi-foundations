program ClipboardDemo;

uses
  FMX.Forms,
  CCR.FMXClipboard in '..\..\CCR.FMXClipboard.pas',
  CCR.FMXClipboard.Apple in '..\..\CCR.FMXClipboard.Apple.pas',
  CCR.FMXClipboard.iOS in '..\..\CCR.FMXClipboard.iOS.pas',
  CCR.FMXClipboard.Mac in '..\..\CCR.FMXClipboard.Mac.pas',
  CCR.FMXClipboard.Win in '..\..\CCR.FMXClipboard.Win.pas',
  ClipboardDemoForm in 'ClipboardDemoForm.pas' {frmClipboardDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmClipboardDemo, frmClipboardDemo);
  Application.Run;
end.

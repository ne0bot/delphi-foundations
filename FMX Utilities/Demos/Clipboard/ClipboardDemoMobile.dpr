program ClipboardDemoMobile;

uses
  System.StartUpCopy,
  FMX.Forms,
  ClipboardDemoFormMobile in 'ClipboardDemoFormMobile.pas' {frmClipboardDemo},
  CCR.FMXClipboard in '..\..\CCR.FMXClipboard.pas',
  CCR.FMXClipboard.Mac in '..\..\CCR.FMXClipboard.Mac.pas',
  CCR.FMXClipboard.Android in '..\..\CCR.FMXClipboard.Android.pas',
  CCR.FMXClipboard.Apple in '..\..\CCR.FMXClipboard.Apple.pas',
  CCR.FMXClipboard.iOS in '..\..\CCR.FMXClipboard.iOS.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmClipboardDemo, frmClipboardDemo);
  Application.Run;
end.

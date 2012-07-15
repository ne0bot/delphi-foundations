program CocoaToolbarDemo;

uses
  FMX.Forms,
  CCR.CocoaToolbar in 'CCR.CocoaToolbar.pas',
  MainForm in 'MainForm.pas' {frmNSToolbarDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmNSToolbarDemo, frmNSToolbarDemo);
  Application.Run;
end.

program ShellOpen;

uses
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmShellOpen};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmShellOpen, frmShellOpen);
  Application.Run;
end.

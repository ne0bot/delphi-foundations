program PackagesDemo;

uses
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  PluginManager in 'PluginManager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

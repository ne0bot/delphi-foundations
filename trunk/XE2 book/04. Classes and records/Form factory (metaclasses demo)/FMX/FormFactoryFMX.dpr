program FormFactoryFMX;

uses
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  ClientForms in 'ClientForms.pas',
  BlueClientForm in 'BlueClientForm.pas' {frmBlueClient},
  RedClientForm in 'RedClientForm.pas' {frmRedClient},
  YellowClientForm in 'YellowClientForm.pas' {frmYellowClient};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

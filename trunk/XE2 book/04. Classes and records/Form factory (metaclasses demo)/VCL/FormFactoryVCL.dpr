program FormFactoryVCL;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  ClientForms in 'ClientForms.pas',
  RedClientForm in 'RedClientForm.pas' {frmRedClient},
  BlueClientForm in 'BlueClientForm.pas' {frmBlueClient},
  YellowClientForm in 'YellowClientForm.pas' {frmYellowClient};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

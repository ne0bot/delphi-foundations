program IniFileIntfDemo;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  AppActions in 'AppActions.pas' {dtmActions: TDataModule},
  LoadedSectionsClasses in 'LoadedSectionsClasses.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdtmActions, dtmActions);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

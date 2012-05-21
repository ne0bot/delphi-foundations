program WinSpecialFolders;

uses
  Vcl.Forms,
  WinMainForm in 'WinMainForm.pas' {frmWinSpecialFolders};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmWinSpecialFolders, frmWinSpecialFolders);
  Application.Run;
end.

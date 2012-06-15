program MacSpecialFolders;

uses
  FMX.Forms,
  MacMainForm in 'MacMainForm.pas' {frmMacSpecialFolders};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMacSpecialFolders, frmMacSpecialFolders);
  Application.Run;
end.

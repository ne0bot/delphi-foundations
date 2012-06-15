program MultiProcessSemaphore;

uses
  Vcl.Forms,
  SemaphoreForm in 'SemaphoreForm.pas' {frmSemaphore};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSemaphore, frmSemaphore);
  Application.Run;
end.

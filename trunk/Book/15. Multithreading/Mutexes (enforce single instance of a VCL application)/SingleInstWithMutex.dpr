program SingleInstWithMutex;

uses
  System.SyncObjs,
  Vcl.Forms,
  SingleInstVCL in 'SingleInstVCL.pas',
  MutexForm in 'MutexForm.pas' {frmMutexDemo};

{$R *.res}

begin
  if not CanRun then Exit;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMutexDemo, frmMutexDemo);
  Application.Run;
end.

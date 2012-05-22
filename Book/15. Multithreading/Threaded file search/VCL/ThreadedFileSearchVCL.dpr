program ThreadedFileSearchVCL;

uses
  Forms,
  FileSearchThread in '..\FileSearchThread.pas',
  FileSearchFormVCL in 'FileSearchFormVCL.pas' {frmFileSearchVCL};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmFileSearchVCL, frmFileSearchVCL);
  Application.Run;
end.

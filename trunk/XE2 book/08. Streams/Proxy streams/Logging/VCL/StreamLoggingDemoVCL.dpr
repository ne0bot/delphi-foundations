program StreamLoggingDemoVCL;

uses
  Vcl.Forms,
  LoggingFormVCL in 'LoggingFormVCL.pas' {frmLogProxyVCL},
  StreamLogging in '..\StreamLogging.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmLogProxyVCL, frmLogProxyVCL);
  Application.Run;
end.

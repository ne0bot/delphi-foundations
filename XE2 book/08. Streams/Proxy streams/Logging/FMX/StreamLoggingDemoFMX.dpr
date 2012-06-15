program StreamLoggingDemoFMX;

uses
  FMX.Forms,
  LoggingFormFMX in 'LoggingFormFMX.pas' {frmLogProxyFMX},
  StreamLogging in '..\StreamLogging.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmLogProxyFMX, frmLogProxyFMX);
  Application.Run;
end.

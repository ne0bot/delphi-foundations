program ThreadedFileSearchFMX;

uses
  FMX.Forms,
  FileSearchFormFMX in 'FileSearchFormFMX.pas' {frmFileSearchFMX},
  FileSearchThread in '..\FileSearchThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmFileSearchFMX, frmFileSearchFMX);
  Application.Run;
end.

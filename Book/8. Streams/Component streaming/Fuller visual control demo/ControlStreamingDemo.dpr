program ControlStreamingDemo;

uses
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmStreamingDemo},
  Containers in 'Containers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmStreamingDemo, frmStreamingDemo);
  Application.Run;
end.

program SynchronizeAndTerminateDemo;

uses
  FMX.Forms,
  ColorSwitchForm in 'ColorSwitchForm.pas' {frmColorSwitcher};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmColorSwitcher, frmColorSwitcher);
  Application.Run;
end.

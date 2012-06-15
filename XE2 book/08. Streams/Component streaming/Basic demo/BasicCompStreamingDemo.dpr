program BasicCompStreamingDemo;

uses
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmBasicStreamingDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmBasicStreamingDemo, frmBasicStreamingDemo);
  Application.Run;
end.

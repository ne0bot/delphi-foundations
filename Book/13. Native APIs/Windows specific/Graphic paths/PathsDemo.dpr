program PathsDemo;

uses
  Vcl.Forms,
  PathsForm in 'PathsForm.pas' {frmPathsDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPathsDemo, frmPathsDemo);
  Application.Run;
end.

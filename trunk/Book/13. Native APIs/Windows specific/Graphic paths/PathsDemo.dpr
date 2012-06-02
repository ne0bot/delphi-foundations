program PathsDemo;

uses
  Vcl.Forms,
  PathsForm in 'PathsForm.pas' {frmPathDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPathDemo, frmPathDemo);
  Application.Run;
end.

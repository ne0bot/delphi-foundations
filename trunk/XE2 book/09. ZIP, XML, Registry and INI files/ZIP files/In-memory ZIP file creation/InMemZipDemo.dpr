program InMemZipDemo;

uses
  FMX.Forms,
  InMemZipForm in 'InMemZipForm.pas' {frmInMemZip};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmInMemZip, frmInMemZip);
  Application.Run;
end.

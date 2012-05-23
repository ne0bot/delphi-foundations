program UnicodeNormalisation;

uses
  FMX.Forms,
  UnicodeNormForm in 'UnicodeNormForm.pas' {frmUnicodeNorm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmUnicodeNorm, frmUnicodeNorm);
  Application.Run;
end.

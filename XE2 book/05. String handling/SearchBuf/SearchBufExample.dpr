program SearchBufExample;

{$R *.dres}

uses
  Forms,
  SearchBufForm in 'SearchBufForm.pas' {frmSearchBufDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSearchBufDemo, frmSearchBufDemo);
  Application.Run;
end.

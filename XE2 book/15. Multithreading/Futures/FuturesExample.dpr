program FuturesExample;

uses
  FMX.Forms,
  FutureEntryForm in 'FutureEntryForm.pas' {frmPrimeNumberTest},
  BetterFutures in 'BetterFutures.pas',
  FutureResultsForm in 'FutureResultsForm.pas' {frmPrimeResults},
  NaiveFutures in 'NaiveFutures.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmPrimeNumberTest, frmPrimeNumberTest);
  Application.Run;
end.

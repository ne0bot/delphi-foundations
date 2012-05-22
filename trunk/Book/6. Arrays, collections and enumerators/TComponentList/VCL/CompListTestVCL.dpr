program CompListTestVCL;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmVCL},
  CCR.Generics.CompList in '..\CCR.Generics.CompList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmVCL, frmVCL);
  Application.Run;
end.

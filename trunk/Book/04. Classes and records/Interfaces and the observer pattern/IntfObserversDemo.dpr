program IntfObserversDemo;

uses
  Vcl.Forms,
  AppEvents in 'AppEvents.pas',
  MainForm in 'MainForm.pas' {frmMain},
  Btn1Observer in 'Btn1Observer.pas' {frmBtn1Observer},
  Btn1And2Observer in 'Btn1And2Observer.pas' {frmBtns1And2Observer},
  ExampleFrame in 'ExampleFrame.pas' {ObserverFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

program ConsoleWithFMXTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.UITypes,
  FMX.Forms,
  FMX.Dialogs,
  NameDlgForm in 'NameDlgForm.pas' {frmNameDlg};

var
  Dlg: TOpenDialog;
  EnteredName: string;
begin
  Application.Title := 'Console with FMX Usage Test';
  Writeln('Demonstration of using a standard dialog box in a console application');
  Dlg := TOpenDialog.Create(nil);
  try
    Dlg.DefaultExt := 'txt';
    Dlg.Filter := 'Text files (*.txt)|*.txt|All files (*.*)|*.*';
    Dlg.Options := [TOpenOption.ofFileMustExist];
    if Dlg.Execute then
      MessageDlg('You chose "' + Dlg.FileName + '"', TMsgDlgType.mtInformation,
        [TMsgDlgBtn.mbOK], 0)
    else
      MessageDlg('You cancelled the dialog!', TMsgDlgType.mtWarning,
        [TMsgDlgBtn.mbOK], 0);
  finally
    Dlg.Free;
  end;
  Writeln('Demonstration of using a FireMonkey form in a console application');
  if ShowNameDialog(EnteredName) then
    ShowMessage('Hello ' + EnteredName)
  else
    ShowMessage('You cancelled by lovely custom dialog - how dare you!');
end.

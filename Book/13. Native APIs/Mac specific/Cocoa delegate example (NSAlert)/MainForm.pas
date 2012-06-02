unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo;

type
  TfrmNSAlert = class(TForm)
    btnDialogStyle: TButton;
    btnSheetStyle: TButton;
    procedure btnSheetStyleClick(Sender: TObject);
    procedure btnDialogStyleClick(Sender: TObject);
  end;

var
  frmNSAlert: TfrmNSAlert;

implementation

uses Macapi.CocoaTypes, Macapi.Foundation, Macapi.AppKit, CCR.NSAlertHelper;

{$R *.fmx}

procedure TfrmNSAlert.btnDialogStyleClick(Sender: TObject);
var
  Alert: NSAlert;
begin
  Alert := TNSAlert.Create;
  try
    Alert.addButtonWithTitle(NSSTR('OK'));
    Alert.addButtonWithTitle(NSSTR('Cancel'));
    Alert.setMessageText(NSSTR('Delete every file on your computer?'));
    Alert.setInformativeText(NSSTR('Deleted files cannot be restored.'));
    Alert.setAlertStyle(NSWarningAlertStyle);
    case Alert.runModal of
      NSAlertFirstButtonReturn: Caption := 'You pressed OK';
      NSAlertSecondButtonReturn: Caption := 'You pressed Cancel';
    else
      Caption := 'Er, something went wrong here...';
    end;
  finally
    Alert.release;
  end;
end;

procedure TfrmNSAlert.btnSheetStyleClick(Sender: TObject);
var
  Alert: NSAlert;
begin
  Alert := TNSAlert.Create;
  Alert.addButtonWithTitle(NSSTR('OK'));
  Alert.addButtonWithTitle(NSSTR('Cancel'));
  Alert.setMessageText(NSSTR('Delete every file on your computer?'));
  Alert.setInformativeText(NSSTR('Deleted files cannot be restored.'));
  Alert.setAlertStyle(NSWarningAlertStyle);
  ShowNSAlertAsSheet(Alert, Self,
    procedure (ReturnCode: NSInteger)
    begin
      case ReturnCode of
        NSAlertFirstButtonReturn: Caption := 'You pressed OK';
        NSAlertSecondButtonReturn: Caption := 'You pressed Cancel';
      else
        Caption := 'Er, something went wrong here...';
      end;
      Alert.release;
    end);
end;

end.

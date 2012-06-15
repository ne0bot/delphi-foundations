unit NameDlgForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit;

type
  TfrmNameDlg = class(TForm)
    Label1: TLabel;
    edtName: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    procedure edtNameKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure edtNameChangeTracking(Sender: TObject);
  end;

function ShowNameDialog(out EnteredName: string): Boolean;

implementation

{$R *.fmx}

function ShowNameDialog(out EnteredName: string): Boolean;
var
  Form: TfrmNameDlg;
begin
  Form := TfrmNameDlg.Create(nil);
  try
    Result := IsPositiveResult(Form.ShowModal);
    if Result then EnteredName := Form.edtName.Text;
  finally
    Form.Free;
  end;
end;

procedure TfrmNameDlg.edtNameChangeTracking(Sender: TObject);
begin
  btnOK.Enabled := (edtName.Text <> '');
end;

procedure TfrmNameDlg.edtNameKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if (Key = vkReturn) and (edtName.Text <> '') then ModalResult := mrOk;
end;

end.

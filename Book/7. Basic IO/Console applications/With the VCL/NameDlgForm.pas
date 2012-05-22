unit NameDlgForm;

interface

uses
  System.SysUtils, System.Classes, System.UITypes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

type
  TfrmNameDlg = class(TForm)
    edtName: TLabeledEdit;
    btnOK: TButton;
    btnCancel: TButton;
    procedure edtNameChange(Sender: TObject);
  end;

function ShowNameDialog(out EnteredName: string): Boolean;

implementation

{$R *.dfm}

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

procedure TfrmNameDlg.edtNameChange(Sender: TObject);
begin
  btnOK.Enabled := (edtName.GetTextLen > 0);
end;

end.

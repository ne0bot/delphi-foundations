unit MainForm;
{
  VCL version of metaclasses demo that is walked through in chapter 4 - please see
  the book for more info.
}
interface

uses
  Winapi.Windows, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TfrmMain = class(TForm)
    lsvForms: TListView;
    panRight: TPanel;
    btnCreateForm: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnCreateFormClick(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

uses ClientForms;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  FormClass: TClientFormClass;
  NewItem: TListItem;
begin
  lsvForms.Items.BeginUpdate;
  try
    for FormClass in RegisteredClientForms do
    begin
      NewItem := lsvForms.Items.Add;
      NewItem.Caption := FormClass.Title;
      NewItem.SubItems.Add(FormClass.Description);
      NewItem.Data := FormClass;
    end;
  finally
    lsvForms.Items.EndUpdate;
  end;
end;

procedure TfrmMain.btnCreateFormClick(Sender: TObject);
var
  NewForm: TForm;
begin
  if lsvForms.SelCount = 0 then
  begin
    Beep;
    Exit;
  end;
  { *Don't* cast to TClientForm, since we put in a metaclass instance, not a
    class instance. }
  NewForm := TClientFormClass(lsvForms.Selected.Data).Create(Self);
  NewForm.Show;
end;

end.

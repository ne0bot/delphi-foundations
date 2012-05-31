unit MainForm;
{
  FMX version of metaclasses demo that is walked through in chapter 4 - please see
  the book for more info.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Grid, FMX.Layouts, FMX.ListBox,
  ClientForms;

type
  TfrmMain = class(TForm)
    lyoLeft: TLayout;
    btnCreateForm: TButton;
    lsbAvailableForms: TListBox;
    lblTitle: TLabel;
    lblDescription: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnCreateFormClick(Sender: TObject);
    procedure lsbAvailableFormsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

type
  TFormListItem = class(TListBoxItem)
  public
    FormClass: TClientFormClass;
  end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  FormClass: TClientFormClass;
  Item: TFormListItem;
  DescLabel: TLabel;
  DescLabelX: Single;
begin
  DescLabelX := lblDescription.Position.X - lsbAvailableForms.Position.X;
  for FormClass in RegisteredClientForms do
  begin
    Item := TFormListItem.Create(Self);
    Item.FormClass := FormClass;
    Item.Text := FormClass.Title;
    DescLabel := TLabel.Create(Item);
    DescLabel.Align := TAlignLayout.alClient;
    DescLabel.Padding.Left := DescLabelX;
    DescLabel.Parent := Item;
    DescLabel.Text := FormClass.Description;
    Item.Parent := lsbAvailableForms;
  end;
  lsbAvailableForms.ItemIndex := 0;
end;

procedure TfrmMain.lsbAvailableFormsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if (ssDouble in Shift) and (lsbAvailableForms.ItemByPoint(X, Y) <> nil) then
    btnCreateFormClick(nil);
end;

procedure TfrmMain.btnCreateFormClick(Sender: TObject);
var
  NewForm: TForm;
  SelItem: TFormListItem;
begin
  SelItem := (lsbAvailableForms.Selected as TFormListItem);
  if SelItem = nil then
  begin
    Beep;
    Exit;
  end;
  NewForm := SelItem.FormClass.Create(Self);
  NewForm.Show;
end;

end.

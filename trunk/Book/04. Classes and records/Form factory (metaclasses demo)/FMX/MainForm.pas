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
    lyoBase: TLayout;
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

procedure TfrmMain.FormCreate(Sender: TObject);
var
  FormClass: TClientFormClass;
begin
  for FormClass in RegisteredClientForms do
    lsbAvailableForms.Items.AddObject(FormClass.Title + #9 +
      FormClass.Description, TObject(FormClass));
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
  SelIndex: Integer;
begin
  SelIndex := lsbAvailableForms.ItemIndex;
  if SelIndex < 0 then
  begin
    Beep;
    Exit;
  end;
  { *Don't* cast to TClientForm, since we put in a metaclass instance, not a
    class instance. }
  NewForm := TClientFormClass(lsbAvailableForms.Items.Objects[SelIndex]).Create(Self);
  NewForm.Show;
end;

end.

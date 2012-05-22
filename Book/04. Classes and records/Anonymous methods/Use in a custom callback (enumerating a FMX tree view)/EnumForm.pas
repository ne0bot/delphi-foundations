unit EnumForm;
{
  Example of using an anonymous method for a custom callback, in this case when
  enumerating a FireMonkey tree view.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TreeView, FMX.Layouts;

type
  TfrmEnumTreeView = class(TForm)
    TreeView1: TTreeView;
    TreeViewItem1: TTreeViewItem;
    TreeViewItem2: TTreeViewItem;
    TreeViewItem3: TTreeViewItem;
    TreeViewItem4: TTreeViewItem;
    TreeViewItem5: TTreeViewItem;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  end;

var
  frmEnumTreeView: TfrmEnumTreeView;

implementation

{$R *.fmx}

{ Declare method reference type. }
type
  TTreeViewItemEnumProc = reference to procedure (AItem: TTreeViewItem; var AContinue: Boolean);

{ Implement the function that does the actual enumerating. A nested function (DoEnum) is used
  to keep things neat and tidy. }
procedure EnumTreeView(ATreeView: TCustomTreeView; const ACallback: TTreeViewItemEnumProc);

  function DoEnum(AItem: TTreeViewItem): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    ACallback(AItem, Result);
    if Result then
      for I := 0 to AItem.Count - 1 do
        if not DoEnum(AItem.Items[I]) then Exit(False);
  end;
var
  I: Integer;
begin
  for I := 0 to ATreeView.Count - 1 do
    if not DoEnum(ATreeView.Items[I]) then Exit;
end;

{ Let's test it out! }
procedure TfrmEnumTreeView.Button1Click(Sender: TObject);
begin
  EnumTreeView(TreeView1,
    procedure (AItem: TTreeViewItem; var AContinue: Boolean)
    begin
      if not IsPositiveResult(MessageDlg(AItem.Text + ' found. Continue enumerating?',
        TMsgDlgType.mtConfirmation, mbOKCancel, 0)) then AContinue := False;
    end);
end;

end.

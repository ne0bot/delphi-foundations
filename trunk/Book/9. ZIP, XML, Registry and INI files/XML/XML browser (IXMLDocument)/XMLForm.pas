unit XMLForm;
{
  Cross platform version of the XML browser demo that uses the 'high level'
  IXMLxxx interfaces. However, in practice, they aren't actually at that much
  higher a level than the IDOMxxx interfaces they wrap.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.TreeView,
  FMX.Memo, FMX.ListBox, XML.XMLIntf;

type
  TfrmWithIXMLDocument = class(TForm)
    Layout1: TLayout;
    btnLoad: TButton;
    trvStructure: TTreeView;
    Splitter1: TSplitter;
    Layout2: TLayout;
    memContent: TMemo;
    dlgOpen: TOpenDialog;
    lblBackend: TLabel;
    cboBackend: TComboBox;
    procedure btnLoadClick(Sender: TObject);
    procedure trvStructureChange(Sender: TObject);
    procedure TreeViewKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormCreate(Sender: TObject);
    procedure cboBackendChange(Sender: TObject);
  strict private
    FDocument: IXMLDocument;
  end;

var
  frmWithIXMLDocument: TfrmWithIXMLDocument;

implementation

uses
  XML.XMLDOM, XML.XMLDoc, XML.ADOMXMLDOM{ ensure ADOM is available on Windows },
  FMXTreeviewUtils;

{$R *.fmx}

type
  TTreeViewNodeItem = class(TTreeViewItem)
    Node: IXMLNode;
  end;

procedure LoadXMLNodeStructure(const ADocument: IXMLDocument; ATreeView: TTreeView);

  procedure AddNode(AParent: TControl; const ANode: IXMLNode);
  var
    Caption: string;
    I: Integer;
    NewItem: TTreeViewNodeItem;
  begin
    case ANode.NodeType of
      ntDocument: Caption := '';
      ntComment: Caption := '[comment]';
      ntProcessingInstr: Caption := ANode.NodeName + ' [processing instruction]';
      ntElement, ntAttribute: Caption := ANode.NodeName;
    else Exit;
    end;
    if Caption <> '' then
    begin
      NewItem := TTreeViewNodeItem.Create(ATreeView);
      NewItem.Node := ANode;
      NewItem.Text := Caption;
      AParent.AddObject(NewItem);
      AParent := NewItem;
    end;
    for I := 0 to ANode.AttributeNodes.Count - 1 do
      AddNode(AParent, ANode.AttributeNodes[I]);
    if ANode.HasChildNodes then
      for I := 0 to ANode.ChildNodes.Count - 1 do
        AddNode(AParent, ANode.ChildNodes[I]);
  end;
begin
  ATreeView.BeginUpdate;
  try
    ATreeView.Clear;
    AddNode(ATreeView, ADocument.Node);
  finally
    ATreeView.EndUpdate;
  end;
end;

procedure TfrmWithIXMLDocument.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to DOMVendors.Count - 1 do
    cboBackend.Items.AddObject(DOMVendors[I].Description, DOMVendors[I]);
  cboBackend.ItemIndex := 0;
end;

procedure TfrmWithIXMLDocument.cboBackendChange(Sender: TObject);
begin
  DefaultDOMVendor := cboBackend.Selected.Text;
end;

procedure TfrmWithIXMLDocument.btnLoadClick(Sender: TObject);
begin
  if not dlgOpen.Execute then Exit;
  FDocument := LoadXMLDocument(dlgOpen.FileName);
  LoadXMLNodeStructure(FDocument, trvStructure);
  if trvStructure.Count > 0 then trvStructure.Selected := trvStructure.Items[0];
end;

procedure TfrmWithIXMLDocument.trvStructureChange(Sender: TObject);
var
  Node: IXMLNode;
begin
  if trvStructure.Selected = nil then Exit;
  Node := (trvStructure.Selected as TTreeViewNodeItem).Node;
  //the Text property raises an exception for element nodes without a text child node
  if Node.NodeType = ntProcessingInstr then
    memContent.Text := ''
  else if (Node.NodeType = ntElement) and (Node.ChildNodes.Count > 0) and
     (Node.ChildNodes[0].NodeType <> ntText) then
    memContent.Text := ''
  else
    memContent.Text := Node.Text;
end;

procedure TfrmWithIXMLDocument.TreeViewKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  HandleTreeViewKeyDown(Sender as TTreeView, Key, KeyChar, Shift);
end;

procedure TfrmWithIXMLDocument.TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  HandleTreeviewMouseDown(Sender as TTreeView, Button, Shift, X, Y);
end;

end.

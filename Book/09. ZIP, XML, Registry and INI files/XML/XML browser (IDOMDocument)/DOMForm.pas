unit DOMForm;
{
  Cross platform version of the XML browser demo that uses the relatively 'lite'
  IDOMxxx interfaces instead of the IXMLxxx ones.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.TreeView,
  FMX.Memo, FMX.ListBox, XML.XMLDOM;

type
  TfrmWithIDOMDocument = class(TForm)
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
    FDocument: IDOMDocument;
  end;

var
  frmWithIDOMDocument: TfrmWithIDOMDocument;

implementation

uses
  XML.ADOMXMLDOM{ ensure ADOM is available on Windows }, FMXTreeviewUtils;

{$R *.fmx}

type
  TTreeViewNodeItem = class(TTreeViewItem)
    Node: IDOMNode;
  end;

procedure LoadXMLNodeStructure(const ADocument: IDOMDocument; ATreeView: TTreeView);

  procedure AddNode(AParent: TControl; const ANode: IDOMNode);
  var
    Caption: string;
    I: Integer;
    NewItem: TTreeViewNodeItem;
  begin
    case ANode.nodeType of
      DOCUMENT_NODE: Caption := '';
      COMMENT_NODE: Caption := '[comment]';
      PROCESSING_INSTRUCTION_NODE: Caption := ANode.nodeName + ' [processing instruction]';
      ELEMENT_NODE, ATTRIBUTE_NODE: Caption := ANode.nodeName;
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
    if ANode.attributes <> nil then
      for I := 0 to ANode.attributes.length - 1 do
        AddNode(AParent, ANode.attributes[I]);
    if ANode.hasChildNodes then
      for I := 0 to ANode.childNodes.length - 1 do
        AddNode(AParent, ANode.childNodes[I]);
  end;
begin
  ATreeView.BeginUpdate;
  try
    ATreeView.Clear;
    AddNode(ATreeView, ADocument);
  finally
    ATreeView.EndUpdate;
  end;
end;

procedure TfrmWithIDOMDocument.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to DOMVendors.Count - 1 do
    cboBackend.Items.AddObject(DOMVendors[I].Description, DOMVendors[I]);
  cboBackend.ItemIndex := 0;
end;

procedure TfrmWithIDOMDocument.cboBackendChange(Sender: TObject);
begin
  DefaultDOMVendor := cboBackend.Selected.Text;
end;

procedure TfrmWithIDOMDocument.btnLoadClick(Sender: TObject);
begin
  if not dlgOpen.Execute then Exit;
  FDocument := GetDOM.createDocument('', '', nil);
  (FDocument as IDOMPersist).load(dlgOpen.FileName);
  LoadXMLNodeStructure(FDocument, trvStructure);
  if trvStructure.Count > 0 then trvStructure.Selected := trvStructure.Items[0];
end;

procedure TfrmWithIDOMDocument.trvStructureChange(Sender: TObject);
var
  Node: IDOMNode;
begin
  if trvStructure.Selected = nil then Exit;
  Node := (trvStructure.Selected as TTreeViewNodeItem).Node;
  //if element node, find the child text node (if it exists)
  case Node.nodeType of
    PROCESSING_INSTRUCTION_NODE: Node := nil;
    ELEMENT_NODE:
    begin
      Node := Node.firstChild;
      while (Node <> nil) and (Node.nodeType <> TEXT_NODE) do
        Node := Node.nextSibling;
    end;
  end;
  if Node = nil then
    memContent.Text := ''
  else
    memContent.Text := Node.nodeValue;
end;

procedure TfrmWithIDOMDocument.TreeViewKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  HandleTreeViewKeyDown(Sender as TTreeView, Key, KeyChar, Shift);
end;

procedure TfrmWithIDOMDocument.TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  HandleTreeviewMouseDown(Sender as TTreeView, Button, Shift, X, Y);
end;

end.

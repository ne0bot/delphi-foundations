unit MSXMLForm;
{
  VCL and MSXML-specific version of the XML browser demo.
}
interface

uses
  Winapi.Windows, Winapi.MSXML, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TfrmMSXML = class(TForm)
    Panel1: TPanel;
    btnLoad: TButton;
    trvOutline: TTreeView;
    Splitter1: TSplitter;
    memContent: TRichEdit;
    dlgOpen: TOpenDialog;
    procedure btnLoadClick(Sender: TObject);
    procedure trvOutlineCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure trvOutlineChange(Sender: TObject; Node: TTreeNode);
  strict private
    FDocument: IXMLDOMDocument2;
  end;

var
  frmMSXML: TfrmMSXML;

implementation

uses
  System.Win.ComObj;

function CreateMSXMLDocument: IXMLDOMDocument2;
begin
  try
    Result := CreateComObject(CLASS_DOMDocument60) as IXMLDOMDocument2;
  except
    on EOleSysError do
      Result := CreateComObject(CLASS_DOMDocument30) as IXMLDOMDocument2;
  end;
  Result.setProperty('NewParser', True);
end;

{$R *.dfm}

type
  TTreeViewNodeItem = class(TTreeNode)
    Node: IXMLDOMNode;
  end;

procedure LoadXMLNodeStructure(const ADocument: IXMLDOMDocument; ATreeView: TTreeView);

  procedure AddNode(AParent: TTreeNode; const ANode: IXMLDOMNode);
  var
    Caption: string;
    I: Integer;
  begin
    case ANode.nodeType of
      NODE_DOCUMENT: Caption := '';
      NODE_COMMENT: Caption := '[comment]';
      NODE_PROCESSING_INSTRUCTION: Caption := ANode.nodeName + ' [processing instruction]';
      NODE_ELEMENT, NODE_ATTRIBUTE: Caption := ANode.nodeName;
    else Exit;
    end;
    if Caption <> '' then
    begin
      AParent := ATreeView.Items.AddChild(AParent, Caption);
      (AParent as TTreeViewNodeItem).Node := ANode;
    end;
    if ANode.attributes <> nil then
      for I := 0 to ANode.attributes.length - 1 do
        AddNode(AParent, ANode.attributes[I]);
    if ANode.hasChildNodes then
      for I := 0 to ANode.childNodes.length - 1 do
        AddNode(AParent, ANode.childNodes[I]);
  end;
begin
  ATreeView.Items.BeginUpdate;
  try
    ATreeView.Items.Clear;
    AddNode(nil, ADocument);
  finally
    ATreeView.Items.EndUpdate;
  end;
end;

procedure TfrmMSXML.btnLoadClick(Sender: TObject);
begin
  if not dlgOpen.Execute then Exit;
  FDocument := CreateMSXMLDocument;
  FDocument.load(dlgOpen.FileName);
  memContent.Clear;
  LoadXMLNodeStructure(FDocument, trvOutline);
end;

procedure TfrmMSXML.trvOutlineChange(Sender: TObject; Node: TTreeNode);
var
  XMLNode: IXMLDOMNode;
begin
  if not Node.Selected then Exit;
  XMLNode := (Node as TTreeViewNodeItem).Node;
  //if element node, find the child text node (if it exists)
  case XMLNode.nodeType of
    NODE_PROCESSING_INSTRUCTION: XMLNode := nil;
    NODE_ELEMENT:
    begin
      XMLNode := XMLNode.firstChild;
      while (XMLNode <> nil) and (XMLNode.nodeType <> NODE_TEXT) do
        XMLNode := XMLNode.nextSibling;
    end;
  end;
  if XMLNode = nil then
    memContent.Text := ''
  else
    memContent.Text := XMLNode.nodeValue;
end;

procedure TfrmMSXML.trvOutlineCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TTreeViewNodeItem;
end;

end.

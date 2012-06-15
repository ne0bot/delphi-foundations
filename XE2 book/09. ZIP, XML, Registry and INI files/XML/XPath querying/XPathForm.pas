unit XPathForm;
{
  Demo of XPath querying using IDOMNodeSelect, which is supported by both the
  MSXML and ADOM backends, and thus, when targeting both Windows and OS X.

  For general info on XPath expressions, see http://www.w3schools.com/XPath/
  While that site assumes a JavaScript-in-the-browser context, since XPath has a
  standard syntax, the discussion applies to the SelectNode and SelectNodes
  methods of a Delphi IDOMNodeSelect interface too. The specific examples in the
  present demo are discussed in my book.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.Layouts,
  FMX.Memo, FMX.ListBox;

type
  TfrmXPath = class(TForm)
    lyoXML: TLayout;
    lblXML: TLabel;
    memXML: TMemo;
    lyoQuery: TLayout;
    Splitter: TSplitter;
    lblBackend: TLabel;
    cboExpr: TComboEdit;
    lyoXPath: TLayout;
    lblMatched: TLabel;
    lyoSpacing: TLayout;
    memOutput: TMemo;
    btnFind: TButton;
    lblExpr: TLabel;
    cboBackend: TComboBox;
    procedure btnFindClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cboExprKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  end;

var
  frmXPath: TfrmXPath;

implementation

uses XML.XMLDOM, XML.ADOMXMLDOM{ have ADOM as an option on Windows };

{$R *.fmx}

function FindChildTextNode(const ANode: IDOMNode; out AChild: IDOMNode): Boolean;
var
  I: Integer;
begin
  if ANode.nodeType <> ELEMENT_NODE then Exit(False);
  for I := ANode.childNodes.length - 1 downto 0 do
  begin
    AChild := ANode.childNodes[I];
    if AChild.nodeType = TEXT_NODE then Exit(True);
  end;
  AChild := nil;
  Result := False;
end;

function NodeTypeToStr(const AType: Word): string;
begin
  case AType of
    ELEMENT_NODE                   : Result := 'element';
    ATTRIBUTE_NODE                 : Result := 'attribute';
    TEXT_NODE                      : Result := 'text';
    CDATA_SECTION_NODE             : Result := 'CData section';
    ENTITY_REFERENCE_NODE          : Result := 'entity reference';
    ENTITY_NODE                    : Result := 'entity';
    PROCESSING_INSTRUCTION_NODE    : Result := 'processing instruction';
    COMMENT_NODE                   : Result := 'comment';
    DOCUMENT_NODE                  : Result := 'document';
    DOCUMENT_TYPE_NODE             : Result := 'document type';
    DOCUMENT_FRAGMENT_NODE         : Result := 'document fragment';
    NOTATION_NODE                  : Result := 'notation';
  else Result := '(unknown)';
  end;
end;

procedure TfrmXPath.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to DOMVendors.Count - 1 do
    cboBackend.Items.Add(DOMVendors[I].Description);
  cboBackend.ItemIndex := 0;
end;

procedure TfrmXPath.cboExprKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if (Key = vkReturn) and (Trim(cboExpr.Text) <> '') then
  begin
    Key := 0;
    btnFindClick(nil);
  end;
end;

procedure TfrmXPath.btnFindClick(Sender: TObject);
var
  Document: IDOMDocument;
  ErrorInfo: IDOMParseError;
  I: Integer;
  Node, TextNode: IDOMNode;
  Nodes: IDOMNodeList;
  S: string;
begin
  { Don't pass anything explicitly to GetDOM, and the platform default DOM
    provider is used. We get the list of available providers in FormCreate. }
  Document := GetDOM(cboBackend.Selected.Text).createDocument('', '', nil);
  if not (Document as IDOMPersist).loadxml(memXML.Text) then
  begin
    ErrorInfo := Document as IDOMParseError;
    memOutput.Text := 'Could not parse XML: ' + ErrorInfo.reason;
    Exit;
  end;
  try
    Nodes := (Document as IDOMNodeSelect).selectNodes(cboExpr.Text);
  except
    on E: Exception do
    begin
      memOutput.Text := 'Error: ' + E.Message;
      Exit;
    end;
  end;
  if (Nodes = nil) or (Nodes.length = 0) then
    memOutput.Text := 'No nodes found'
  else
  begin
    for I := 0 to Nodes.length - 1 do
    begin
      Node := Nodes[I];
      S := S + 'Type: ' + NodeTypeToStr(Node.nodeType) + ', ' +
               'name: "' + Node.nodeName + '", ';
      if FindChildTextNode(Node, TextNode) then
        S := S + 'text value: "' + TextNode.nodeValue + '"'
      else
        S := S + 'value: "' + Node.nodeValue + '"';
      S := S + sLineBreak;
    end;
    memOutput.Text := S;
  end;
end;

end.

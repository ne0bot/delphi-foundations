unit ChooseNodeForm;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Xml.XmlIntf;

type
  TfrmChooseNode = class(TForm)
    lblPrompt: TLabel;
    cboNodes: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
  end;

function ChooseRootNode(const ADocument: IXMLDocument; out ANode: IXMLNode): Boolean;

implementation

{$R *.dfm}

function ChooseRootNode(const ADocument: IXMLDocument; out ANode: IXMLNode): Boolean;
var
  Form: TfrmChooseNode;
  I: Integer;
begin
  Form := TfrmChooseNode.Create(nil);
  try
    for I := 0 to ADocument.ChildNodes.Count - 1 do
      Form.cboNodes.Items.Add(ADocument.ChildNodes[I].NodeName);
    Result := IsPositiveResult(Form.ShowModal);
    if not Result then Exit;
    if Form.cboNodes.ItemIndex = 0 then
      ANode := ADocument.DocumentElement
    else
      ANode := ADocument.ChildNodes[Form.cboNodes.ItemIndex - 1];
  finally
    Form.Free;
  end;
end;

end.

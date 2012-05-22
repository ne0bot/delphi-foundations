unit LoggingFormFMX;
{
  FMX demo of a proxy stream class that logs a base stream's reads, writes and
  seeks to a TStrings object. This is the simplest sort of TStream descendant
  you can write, proxy or otherwise.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo, FMX.Grid, FMX.ListBox;

type
  TfrmLogProxyFMX = class(TForm)
    Layout1: TLayout;
    btnLoadXML: TButton;
    grpXML: TGroupBox;
    Splitter1: TSplitter;
    grpLog: TGroupBox;
    dlgOpen: TOpenDialog;
    grdLog: TGrid;
    colLog: TColumn;
    StyleBook: TStyleBook;
    grdXML: TGrid;
    colXML: TColumn;
    Label1: TLabel;
    cboDOMBackend: TComboBox;
    procedure btnLoadXMLClick(Sender: TObject);
    procedure grdLogGetValue(Sender: TObject; const Col, Row: Integer;
      var Value: Variant);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure grdXMLGetValue(Sender: TObject; const Col, Row: Integer;
      var Value: Variant);
    procedure GridResize(Sender: TObject);
    procedure cboDOMBackendChange(Sender: TObject);
  strict private
    FLogStrings, FXMLStrings: TStringList;
    procedure LoadXML(const AFileName: string);
  end;

var
  frmLogProxyFMX: TfrmLogProxyFMX;

implementation

{$R *.fmx}

uses Xml.XmlDom, Xml.adomxmldom, StreamLogging;

procedure TfrmLogProxyFMX.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to DOMVendors.Count - 1 do
    cboDOMBackend.Items.Add(DOMVendors[I].Description);
  cboDOMBackend.ItemIndex := 0;
  FLogStrings := TStringList.Create;
  FXMLStrings := TStringList.Create;
  GridResize(grdLog);
  GridResize(grdXML);
end;

procedure TfrmLogProxyFMX.FormDestroy(Sender: TObject);
begin
  FLogStrings.Free;
  FXMLStrings.Free;
end;

procedure TfrmLogProxyFMX.LoadXML(const AFileName: string);
var
  Document: IDOMPersist;
  Stream: TStream;
begin
  Document := GetDOM(cboDOMBackend.Selected.Text).createDocument('', '', nil) as IDOMPersist;
  FLogStrings.Clear;
  try
    FLogStrings.Add('Beginning parsing ' + DateTimeToStr(Now));
    Stream := TProxyLogStream.Create(FLogStrings,
      TFileStream.Create(AFileName, fmOpenRead), True);
    try
      Document.loadFromStream(Stream);
    finally
      Stream.Free;
    end;
    FLogStrings.Add('Ended parsing ' + DateTimeToStr(Now));
  finally
    grdLog.RowCount := FLogStrings.Count;
  end;
  FXMLStrings.Text := Document.xml;
  grdXML.RowCount := FXMLStrings.Count;
end;

procedure TfrmLogProxyFMX.btnLoadXMLClick(Sender: TObject);
begin
  if dlgOpen.Execute then LoadXML(dlgOpen.FileName);
end;

procedure TfrmLogProxyFMX.cboDOMBackendChange(Sender: TObject);
begin
  if (dlgOpen.FileName <> '') and (MessageDlg('Reload XML?', TMsgDlgType.mtConfirmation,
    mbOKCancel, 0) = mrOK) then LoadXML(dlgOpen.FileName);
end;

procedure TfrmLogProxyFMX.grdLogGetValue(Sender: TObject; const Col, Row: Integer;
  var Value: Variant);
begin
  Value := FLogStrings[Row];
end;

procedure TfrmLogProxyFMX.grdXMLGetValue(Sender: TObject; const Col, Row: Integer;
  var Value: Variant);
begin
  Value := FXMLStrings[Row];
end;

procedure TfrmLogProxyFMX.GridResize(Sender: TObject);
var
  Grid: TGrid;
begin
  Grid := (Sender as TGrid);
  Grid.Columns[0].Width := Grid.Width - 20;
end;

end.

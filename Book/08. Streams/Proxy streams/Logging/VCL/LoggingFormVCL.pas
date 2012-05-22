unit LoggingFormVCL;
{
  VCL demo of a proxy stream class that logs a base stream's reads, writes and
  seeks to a TStrings object. This is the simplest sort of TStream descendant
  you can write, proxy or otherwise.
}

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TfrmLogProxyVCL = class(TForm)
    Panel1: TPanel;
    btnLoadXML: TButton;
    grpLog: TGroupBox;
    lsbLog: TListBox;
    Splitter1: TSplitter;
    grpXML: TGroupBox;
    memXML: TRichEdit;
    dlgOpen: TOpenDialog;
    Label1: TLabel;
    cboDOMBackend: TComboBox;
    procedure btnLoadXMLClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lsbLogData(Control: TWinControl; Index: Integer;
      var Data: string);
    procedure cboDOMBackendChange(Sender: TObject);
  strict private
    FLogStrings: TStringList;
    procedure LoadXML(const AFileName: string);
  end;

var
  frmLogProxyVCL: TfrmLogProxyVCL;

implementation

{$R *.dfm}

uses Xml.XmlDom, Xml.adomxmldom, StreamLogging;

procedure TfrmLogProxyVCL.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to DOMVendors.Count - 1 do
    cboDOMBackend.Items.Add(DOMVendors[I].Description);
  cboDOMBackend.ItemIndex := 0;
  FLogStrings := TStringList.Create;
end;

procedure TfrmLogProxyVCL.FormDestroy(Sender: TObject);
begin
  FLogStrings.Free;
end;

procedure TfrmLogProxyVCL.LoadXML(const AFileName: string);
var
  Document: IDOMPersist;
  Stream: TStream;
begin
  Document := GetDOM(cboDOMBackend.Text).createDocument('', '', nil) as IDOMPersist;
  FLogStrings.Clear;
  try
    Screen.Cursor := crHourGlass;
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
    lsbLog.Count := FLogStrings.Count;
    Screen.Cursor := crDefault;
  end;
  memXML.Text := Document.xml;
end;

procedure TfrmLogProxyVCL.cboDOMBackendChange(Sender: TObject);
begin
  if (dlgOpen.FileName <> '') and (MessageDlg('Reload XML?', mtConfirmation,
    mbOKCancel, 0) = mrOK) then LoadXML(dlgOpen.FileName);
end;

procedure TfrmLogProxyVCL.lsbLogData(Control: TWinControl; Index: Integer;
  var Data: string);
begin
  Data := FLogStrings[Index];
end;

procedure TfrmLogProxyVCL.btnLoadXMLClick(Sender: TObject);
begin
  if not dlgOpen.Execute then Exit;
  btnLoadXML.Update; //if it wants to repaint, let it
  LoadXML(dlgOpen.FileName);
end;

end.

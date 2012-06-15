unit XMLForm;
{
  Simple demo of the stock ZLib proxy streams to compress an arbitrary XML file.
  Compressed files are saved with a made up .xmlc extention - raw compressed
  ZLib data isn't a recognised format as such.
}
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TfrmXML = class(TForm)
    Panel1: TPanel;
    btnLoadSource: TButton;
    btnCalcCompressedSize: TButton;
    btnResaveCompressed: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    memXML: TRichEdit;
    procedure btnLoadSourceClick(Sender: TObject);
    procedure btnCalcCompressedSizeClick(Sender: TObject);
    procedure btnResaveCompressedClick(Sender: TObject);
  strict private
    FSourceFileName: string;
    procedure LoadXMLStream(Stream: TStream);
    procedure LoadXMLFile(const AFileName: string);
    procedure SaveCompressedXML(ADest: TStream);
  end;

var
  frmXML: TfrmXML;

implementation

uses Xml.XMLDOM, System.ZLib;

{$R *.dfm}

procedure TfrmXML.LoadXMLStream(Stream: TStream);
var
  DOMDoc: IDOMDocument;
begin
  //use the RTL's DOM support so we don't have to bother with encoding issues ourselves
  DOMDoc := GetDOM.createDocument('', '', nil);
  (DOMDoc as IDOMPersist).loadFromStream(Stream);
  memXML.Text := (DOMDoc as IDOMPersist).xml;
end;

procedure TfrmXML.LoadXMLFile(const AFileName: string);
var
  AlreadyCompressed: Boolean;
  FileStream, ProxyStream: TStream;
begin
  AlreadyCompressed := SameFileName(ExtractFileExt(AFileName), '.xmlc');
  FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    //check the file name extension to determine whether to use a proxy stream or not
    if AlreadyCompressed then
    begin
      ProxyStream := TZDecompressionStream.Create(FileStream);
      try
        LoadXMLStream(ProxyStream);
      finally
        ProxyStream.Free;
      end;
      Caption := Format('%s - already compressed', [ExtractFileName(AFileName)]);
    end
    else
    begin
      LoadXMLStream(FileStream);
      Caption := Format('%s - %.0nKB uncompressed', [ExtractFileName(AFileName), FileStream.Size / 1024]);
    end;
  finally
    FileStream.Free;
  end;
  FSourceFileName := AFileName;
  btnCalcCompressedSize.Enabled := not AlreadyCompressed;
  btnResaveCompressed.Enabled := not AlreadyCompressed;
end;

procedure TfrmXML.SaveCompressedXML(ADest: TStream);
var
  Proxy: TZCompressionStream;
  Source: TFileStream;
begin
  Proxy := nil;
  Source := TFileStream.Create(FSourceFileName, fmOpenRead or fmShareDenyWrite);
  try
    Proxy := TZCompressionStream.Create(clMax, ADest);
    Proxy.CopyFrom(Source, 0); //copy the whole file
  finally
    Proxy.Free;
    Source.Free;
  end;
end;

procedure TfrmXML.btnLoadSourceClick(Sender: TObject);
begin
  if dlgOpen.Execute then LoadXMLFile(dlgOpen.FileName);
end;

procedure TfrmXML.btnCalcCompressedSizeClick(Sender: TObject);
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    SaveCompressedXML(MemStream);
    ShowMessageFmt('Compressed size is %.0nKB', [MemStream.Size / 1024]);
  finally
    MemStream.Free;
  end;
end;

procedure TfrmXML.btnResaveCompressedClick(Sender: TObject);
var
  Stream: TFileStream;
begin
  if not dlgSave.Execute then Exit;
  Stream := TFileStream.Create(dlgSave.FileName, fmCreate);
  try
    SaveCompressedXML(Stream);
  finally
    Stream.Free;
  end;
end;

end.

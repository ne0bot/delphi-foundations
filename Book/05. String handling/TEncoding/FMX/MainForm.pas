unit MainForm;
{
  FireMonkey version of a TEncoding demo.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo,
  FMX.ListBox, CCR.Encodings;

type
  TfrmEncodingsTest = class(TForm)
    Memo: TMemo;
    grpDetectionMethod: TGroupBox;
    rdoStandardDetection: TRadioButton;
    rdoCustomDetection: TRadioButton;
    lyoBase: TLayout;
    lyoSidePanel: TLayout;
    grpDefaultEncoding: TGroupBox;
    rdoSystemDefault: TRadioButton;
    rdoGreekAnsiDefault: TRadioButton;
    rdoLatin1Default: TRadioButton;
    rdoBOMlessUTF8Default: TRadioButton;
    grpTestFiles: TGroupBox;
    lsbTestFiles: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure lsbTestFilesClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rdoStandardDetectionChange(Sender: TObject);
    procedure rdoCustomDetectionChange(Sender: TObject);
    procedure rdoDefaultChange(Sender: TObject);
    procedure itmCutClick(Sender: TObject);
    procedure itmCopyClick(Sender: TObject);
    procedure itmPasteClick(Sender: TObject);
  strict private
    FDetectableEncodings: array [TDetectedEncodingType] of TEncoding;
    FGreekAnsiEncoding, FWinLatin1Encoding: TEncoding;
    FTextFilesPath: string;
    FUseCustomMethod: Boolean;
    procedure ReloadTextFile(NewDefaultEncoding: TEncoding = nil);
  end;

var
  frmEncodingsTest: TfrmEncodingsTest;

implementation

{$R *.fmx}

procedure TfrmEncodingsTest.FormCreate(Sender: TObject);
var
  I, PathDelimsToFind: Integer;
  SearchRec: TSearchRec;
begin
  ReportMemoryLeaksOnShutdown := True;
  //initialise the possible TEncoding instances to be used
  FDetectableEncodings[deUnrecognized] := TEncoding.Default;
  FDetectableEncodings[deUTF8] := TEncoding.UTF8;
  FDetectableEncodings[deUTF8NoBOM] := TUTF8EncodingEx.Create([eoAllowInvalidChars]);
  FDetectableEncodings[deUTF16LE] := TEncoding.Unicode;
  FDetectableEncodings[deUTF16BE] := TEncoding.BigEndianUnicode;
  FDetectableEncodings[deUTF32LE] := TUTF32Encoding.Create;
  FDetectableEncodings[deUTF32BE] := TBigEndianUTF32Encoding.Create;
  FGreekAnsiEncoding := TEncoding.GetEncoding(1253);
  FWinLatin1Encoding := TEncoding.GetEncoding(1252);
  //assign the radio buttons' TagObject props (we use common event handlers)
  rdoSystemDefault.TagObject := TEncoding.Default;
  rdoGreekAnsiDefault.TagObject := FGreekAnsiEncoding;
  rdoLatin1Default.TagObject := FWinLatin1Encoding;
  rdoBOMlessUTF8Default.TagObject := FDetectableEncodings[deUTF8NoBOM];
  //find the example text files
  FTextFilesPath := ExtractFilePath(GetModuleName(0));
  for I := 4 downto 0 do
    if FindFirst(FTextFilesPath + '*.txt', faAnyFile and not faDirectory, SearchRec) = 0 then
      Break
    else if I = 0 then
    begin
      MessageDlg('Cannot find example text files', TMsgDlgType.mtError,
        [TMsgDlgBtn.mbOK], 0);
      Application.Terminate;
      Exit;
    end
    else
    begin
      Delete(FTextFilesPath, Length(FTextFilesPath), 1); //delete the trailing path delimiter
      FTextFilesPath := ExtractFilePath(FTextFilesPath)
    end;
  repeat
    lsbTestFiles.Items.Add(SearchRec.Name);
  until FindNext(SearchRec) <> 0;
  FindClose(SearchRec);
end;

procedure TfrmEncodingsTest.FormDestroy(Sender: TObject);
var
  Encoding: TEncoding;
begin
  FGreekAnsiEncoding.Free;
  FWinLatin1Encoding.Free;
  for Encoding in FDetectableEncodings do
    if not TEncoding.IsStandardEncoding(Encoding) then Encoding.Free;
end;

procedure TfrmEncodingsTest.ReloadTextFile(NewDefaultEncoding: TEncoding = nil);
var
  DetectedType: TDetectedEncodingType;
  FileName: string;
begin
  if NewDefaultEncoding <> nil then Memo.Lines.DefaultEncoding := NewDefaultEncoding;
  if lsbTestFiles.ItemIndex >= 0 then
  begin
    FileName := FTextFilesPath + lsbTestFiles.Items[lsbTestFiles.ItemIndex];
    if not FUseCustomMethod then
      Memo.Lines.LoadFromFile(FileName)
    else
    begin
      DetectedType := DetectEncoding(FileName);
      if DetectedType <> deUnrecognized then
        Memo.Lines.LoadFromFile(FileName, FDetectableEncodings[DetectedType])
      else
        Memo.Lines.LoadFromFile(FileName, Memo.Lines.DefaultEncoding);
    end;
  end;
end;

procedure TfrmEncodingsTest.rdoStandardDetectionChange(Sender: TObject);
begin
  FUseCustomMethod := False;
  ReloadTextFile;
end;

procedure TfrmEncodingsTest.rdoCustomDetectionChange(Sender: TObject);
begin
  FUseCustomMethod := True;
  ReloadTextFile;
end;

procedure TfrmEncodingsTest.rdoDefaultChange(Sender: TObject);
begin
  ReloadTextFile((Sender as TFmxObject).TagObject as TEncoding);
end;

procedure TfrmEncodingsTest.lsbTestFilesClick(Sender: TObject);
begin
  ReloadTextFile;
end;

procedure TfrmEncodingsTest.itmCopyClick(Sender: TObject);
begin
  Memo.CopyToClipboard;
end;

procedure TfrmEncodingsTest.itmCutClick(Sender: TObject);
begin
  Memo.CutToClipboard;
end;

procedure TfrmEncodingsTest.itmPasteClick(Sender: TObject);
begin
  Memo.PasteFromClipboard;
end;

end.

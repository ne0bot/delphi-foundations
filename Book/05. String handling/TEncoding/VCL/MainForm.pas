unit MainForm;
{
  Simple test bed for using TEncoding to load different text files with different
  encodings. See the text in the form's memo control for more info.
}
interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ButtonGroup, Vcl.CategoryButtons, CCR.Encodings;

type
  TfrmEncodingsTest = class(TForm)
    Memo: TMemo;
    panOptions: TPanel;
    catFiles: TCategoryButtons;
    ImageList: TImageList;
    catDetectionMethod: TCategoryButtons;
    catDefaultEncoding: TCategoryButtons;
    procedure FormCreate(Sender: TObject);
    procedure btnGreekAnsiDefaultClick(Sender: TObject);
    procedure catFilesButtonClick(Sender: TObject);
    procedure btnBOMlessUTF8DefaultClick(Sender: TObject);
    procedure btnStdDetectionClick(Sender: TObject);
    procedure btnCustomDetectionClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSysDefaultClick(Sender: TObject);
    procedure btnLatin1DefaultClick(Sender: TObject);
    procedure CategoryCollapse(Sender: TObject; const Category: TButtonCategory);
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

{$R *.dfm}

procedure TfrmEncodingsTest.FormCreate(Sender: TObject);
var
  I, PathDelimsToFind: Integer;
  SearchRec: TSearchRec;
begin
  ReportMemoryLeaksOnShutdown := True;
  Constraints.MinHeight := Height;
  catDetectionMethod.SelectedItem := catDetectionMethod.Categories[0].Items[0];
  catDefaultEncoding.SelectedItem := catDefaultEncoding.Categories[0].Items[0];
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
  //load the list of example text files from the source folder
  FTextFilesPath := ParamStr(0);
  PathDelimsToFind := 4;
  for I := Length(FTextFilesPath) - 1 downto 1 do
    if FTextFilesPath[I] = PathDelim then
    begin
      Dec(PathDelimsToFind);
      if PathDelimsToFind = 0 then
      begin
        FTextFilesPath := Copy(FTextFilesPath, 1, I);
        Break;
      end;
    end;
  if FindFirst(FTextFilesPath + '*.txt', faAnyFile and not faDirectory, SearchRec) <> 0 then
  begin
    MessageDlg('Cannot find example text files', mtError, [mbOK], 0);
    Application.ShowMainForm := False;
    Application.Terminate;
    Exit;
  end;
  repeat
    with catFiles.Categories[0].Items.Add do
    begin
      Caption := SearchRec.Name;
      Hint := FTextFilesPath + Caption;
      ImageIndex := 0;
      OnClick := catFilesButtonClick;
    end;
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
  if catFiles.SelectedItem is TButtonItem then
  begin
    FileName := FTextFilesPath + catFiles.SelectedItem.Caption;
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
  Memo.SetFocus;
end;

procedure TfrmEncodingsTest.btnStdDetectionClick(Sender: TObject);
begin
  FUseCustomMethod := False;
  ReloadTextFile;
end;

procedure TfrmEncodingsTest.btnCustomDetectionClick(Sender: TObject);
begin
  FUseCustomMethod := True;
  ReloadTextFile;
end;

procedure TfrmEncodingsTest.btnSysDefaultClick(Sender: TObject);
begin
  ReloadTextFile(TEncoding.Default);
end;

procedure TfrmEncodingsTest.btnGreekAnsiDefaultClick(Sender: TObject);
begin
  ReloadTextFile(FGreekAnsiEncoding);
end;

procedure TfrmEncodingsTest.btnLatin1DefaultClick(Sender: TObject);
begin
  ReloadTextFile(FWinLatin1Encoding);
end;

procedure TfrmEncodingsTest.btnBOMlessUTF8DefaultClick(Sender: TObject);
begin
  ReloadTextFile(FDetectableEncodings[deUTF8NoBOM]);
end;

procedure TfrmEncodingsTest.catFilesButtonClick(Sender: TObject);
begin
  ReloadTextFile;
end;

procedure TfrmEncodingsTest.CategoryCollapse(Sender: TObject; const Category: TButtonCategory);
begin
  Category.Collapsed := False; //un-collapse it!
end;

end.

unit MainForm;
{
  A cross platform ZIP file browser, using TZipFile.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.ListBox,
  FMX.TreeView, FMX.Grid, FMX.Edit, System.Zip;

type
  TfrmZipFileBrowser = class(TForm)
    trvFolders: TTreeView;
    lsbZipFiles: TListBox;
    VertSplitter: TSplitter;
    grpFolders: TGroupBox;
    grpFiles: TGroupBox;
    grpZipContents: TGroupBox;
    HorzSplitter: TSplitter;
    lyoZipFile: TLayout;
    lblUTF8: TLabel;
    lblComment: TLabel;
    edtUTF8: TEdit;
    lblZippedFileCount: TLabel;
    edtZippedFileCount: TEdit;
    edtComment: TEdit;
    stgZippedFiles: TStringGrid;
    stcZippedFilePath: TStringColumn;
    stcZippedFileComment: TStringColumn;
    stcCompressedSize: TStringColumn;
    sgcUncompressedSize: TStringColumn;
    stcLastModified: TStringColumn;
    stcCompressionMethod: TStringColumn;
    procedure FormCreate(Sender: TObject);
    procedure trvFoldersChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lsbZipFilesChange(Sender: TObject);
    procedure stgZippedFilesDblClick(Sender: TObject);
    procedure stgZippedFilesKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
  strict private
    FBaseTempDir: string;
    FZipFile: TZipFile;
    procedure CheckLoadDirectories(AParent: TFmxObject; ALevels: Integer = 2);
    function SelectedZipFileName: string;
  end;

var
  frmZipFileBrowser: TfrmZipFileBrowser;

implementation

uses System.IOUtils, System.StrUtils, FileUtils;

{$R *.fmx}

resourcestring
  SShellOpenFailed = 'Could not open %s - perhaps a relevant file association has not been set up.';

procedure TfrmZipFileBrowser.FormCreate(Sender: TObject);
begin
  Application.Title := Caption;
  FBaseTempDir := GetLocalTempDir;
  FZipFile := TZipFile.Create;
  trvFolders.TagString := GetRealHomeDir;
  CheckLoadDirectories(trvFolders);
end;

procedure TfrmZipFileBrowser.FormDestroy(Sender: TObject);
begin
  FZipFile.Free;
  if DirectoryExists(FBaseTempDir) then TDirectory.Delete(FBaseTempDir, True);
end;

procedure TfrmZipFileBrowser.CheckLoadDirectories(AParent: TFmxObject; ALevels: Integer);
var
  HaveUpdated: Boolean;
  NewItem: TTreeViewItem;
  Path: string;
  SearchRec: TSearchRec;
begin
  if (ALevels < 1) or (AParent.Tag = 1) then Exit;
  Path := IncludeTrailingPathDelimiter(AParent.TagString);
  HaveUpdated := False;
  try
    if FindFirst(Path + '*', faDirectory, SearchRec) = 0 then
    begin
      repeat
        if (SearchRec.Attr and faDirectory <> 0) and (SearchRec.Name <> '.') and
           (SearchRec.Name <> '..') then
        begin
          if not HaveUpdated then
          begin
            trvFolders.BeginUpdate;
            HaveUpdated := True;
          end;
          NewItem := TTreeViewItem.Create(trvFolders);
          NewItem.Parent := AParent;
          NewItem.TagString := Path + SearchRec.Name;
          NewItem.Text := SearchRec.Name;
          CheckLoadDirectories(NewItem, ALevels - 1);
        end;
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
  finally
    if HaveUpdated then trvFolders.EndUpdate;
  end;
  AParent.Tag := 1;
end;

function TfrmZipFileBrowser.SelectedZipFileName: string;
begin
  Result := lsbZipFiles.Selected.TagString;
end;

procedure TfrmZipFileBrowser.trvFoldersChange(Sender: TObject);
const
  AllFilesMask = {$IFDEF MSWINDOWS}'*.*'{$ELSE}'*'{$ENDIF};
var
  Item: TListBoxItem;
  Path: string;
  Rec: TSearchRec;
begin
  if trvFolders.Selected = nil then Exit;
  CheckLoadDirectories(trvFolders.Selected);
  Path := IncludeTrailingPathDelimiter(trvFolders.Selected.TagString);
  lsbZipFiles.BeginUpdate;
  try
    lsbZipFiles.Clear;
    if FindFirst(Path + AllFilesMask, 0, Rec) = 0 then
    try
      repeat
        if MatchText(ExtractFileExt(Rec.Name), ZipFileExts) then
        begin
          Item := TListBoxItem.Create(lsbZipFiles);
          Item.Parent := lsbZipFiles;
          Item.Text := Rec.Name;
          Item.TagString := Path + Rec.Name;
        end;
      until FindNext(Rec) <> 0;
    finally
      FindClose(Rec);
    end;
  finally
    lsbZipFiles.EndUpdate;
  end;
end;

procedure TfrmZipFileBrowser.lsbZipFilesChange(Sender: TObject);

  function SizeStr(const ASize: UInt32): string;
  begin
    if ASize < 1024 then
      FmtStr(Result, '%d bytes', [ASize])
    else
      FmtStr(Result, '%.0n KB', [ASize / 1024]);
  end;

var
  DT: TDateTime;
  I: Integer;
begin
  if (lsbZipFiles.ItemIndex >= 0) and TZipFile.IsValid(lsbZipFiles.Selected.TagString) then
  begin
    FZipFile.Open(SelectedZipFileName, zmRead);
    if FZipFile.UTF8Support then edtUTF8.Text := 'Yes' else edtUTF8.Text := 'No';
    edtZippedFileCount.Text := IntToStr(FZipFile.FileCount);
    edtComment.Text := FZipFile.Comment;
    stgZippedFiles.RowCount := FZipFile.FileCount;
    for I := 0 to FZipFile.FileCount - 1 do
    begin
      stgZippedFiles.Cells[0, I] := FZipFile.FileName[I];
      with FZipFile.FileInfo[I] do
      begin
        stgZippedFiles.Cells[1, I] := SizeStr(CompressedSize);
        stgZippedFiles.Cells[2, I] := SizeStr(UncompressedSize);
        if TryDOSFileDateToDateTime(ModifiedDateTime, DT) then
          stgZippedFiles.Cells[3, I] := DateTimeToStr(DT)
        else
          stgZippedFiles.Cells[3, I] := '(invalid)';
        stgZippedFiles.Cells[4, I] := TZipCompressionToString(TZipCompression(CompressionMethod));
      end;
      stgZippedFiles.Cells[5, I] := FZipFile.FileComment[I];
    end;
  end
  else
  begin
    FZipFile.Close;
    edtUTF8.Text := '';
    edtZippedFileCount.Text := '';
    if lsbZipFiles.ItemIndex >= 0 then
      edtComment.Text := '(invalid ZIP file)'
    else
      edtComment.Text := '';
    stgZippedFiles.RowCount := 0;
  end;
end;

procedure TfrmZipFileBrowser.stgZippedFilesDblClick(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  I := stgZippedFiles.Selected;
  if I < 0 then Exit;
  S := FBaseTempDir + PathDelim + lsbZipFiles.Selected.Text;
  FZipFile.Extract(I, S);
  S := TPath.Combine(S, FZipFile.FileName[stgZippedFiles.Selected]);
  {$IFDEF MSWINDOWS}
  for I := Length(S) downto 1 do
    if S[I] = '/' then S[I] := '\';
  {$ENDIF}
  if not ShellOpen(S) then
    MessageDlg(Format(SShellOpenFailed,
      [ExtractFileName(S)]), TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
end;

procedure TfrmZipFileBrowser.stgZippedFilesKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    Key := 0;
    stgZippedFilesDblClick(nil);
  end;
end;

end.

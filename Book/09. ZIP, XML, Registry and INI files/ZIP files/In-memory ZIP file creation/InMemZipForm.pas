unit InMemZipForm;
{
  Simple demo of creating a ZIP file purely from things in memory rather than
  from files on disk, and then extracting things from a ZIP to in-memory objects
  too. Much better than messing around with temporary files!
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Objects, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts,
  FMX.Edit, FMX.Memo;

type
  TfrmInMemZip = class(TForm)
    lblTextFile: TLabel;
    memTextFile: TMemo;
    lblTextFileName: TLabel;
    edtTextFileName: TEdit;
    lblPicture: TLabel;
    imgPicture: TImageControl;
    edtPictureFileName: TEdit;
    lblPictureFileName: TLabel;
    btnSaveToMem: TButton;
    btnRestoreFromMem: TButton;
    btnSaveToFile: TButton;
    btnClose: TButton;
    dlgSave: TSaveDialog;
    procedure btnCloseClick(Sender: TObject);
    procedure btnSaveToMemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSaveToFileClick(Sender: TObject);
    procedure btnRestoreFromMemClick(Sender: TObject);
    procedure edtFileNameApplyStyleLookup(Sender: TObject);
  strict private
    FZipStream: TMemoryStream;
    procedure SaveToZipStream(Stream: TStream);
  end;

var
  frmInMemZip: TfrmInMemZip;

implementation

uses
  System.Zip;

{$R *.fmx}

procedure TfrmInMemZip.FormCreate(Sender: TObject);
begin
  FZipStream := TMemoryStream.Create;
end;

procedure TfrmInMemZip.FormDestroy(Sender: TObject);
begin
  FZipStream.Free;
end;

procedure TfrmInMemZip.edtFileNameApplyStyleLookup(Sender: TObject);
begin
  //give the edit box a light gray background to indicate its read-only status
  ((Sender as TEdit).FindStyleResource('background') as TShape).Fill.Color := $FFEEEEEE;
end;

procedure TfrmInMemZip.SaveToZipStream(Stream: TStream);
var
  TempStream: TMemoryStream;
  ZipFile: TZipFile;
begin
  ZipFile := TZipFile.Create;
  try
    ZipFile.Open(Stream, zmWrite);
    //add the text 'file'
    ZipFile.Add(TEncoding.UTF8.GetBytes(memTextFile.Text), edtTextFileName.Text);
    //add the PNG image
    TempStream := TMemoryStream.Create;
    try
      imgPicture.Bitmap.SaveToStream(TempStream);
      TempStream.Position := 0;
      ZipFile.Add(TempStream, edtPictureFileName.Text);
    finally
      TempStream.Free;
    end;
    //call Close so that TZipFile can write out the file structure (Close is
    //actually called by the destructor, but we'll be explicit)
    ZipFile.Close;
  finally
    ZipFile.Free;
  end;
end;

procedure TfrmInMemZip.btnSaveToMemClick(Sender: TObject);
begin
  FZipStream.Clear;
  btnRestoreFromMem.Enabled := False; //in case of an exception
  SaveToZipStream(FZipStream);
  btnRestoreFromMem.Enabled := True;
  if not IsPositiveResult(MessageDlg('Saved to memory ZIP. Clear data?',
    TMsgDlgType.mtConfirmation, mbOKCancel, 0)) then Exit;
  memTextFile.Text := '';
  imgPicture.Bitmap.SetSize(0, 0);
end;

procedure TfrmInMemZip.btnRestoreFromMemClick(Sender: TObject);
var
  Bytes: TBytes;
  TempStream: TBytesStream;
  ZipFile: TZipFile;
begin
  FZipStream.Position := 0;
  ZipFile := TZipFile.Create;
  try
    ZipFile.Open(FZipStream, zmRead);
    //read the text 'file'...
    ZipFile.Read(edtTextFileName.Text, Bytes);
    memTextFile.Text := TEncoding.UTF8.GetString(Bytes);
    //read the PNG image...
    ZipFile.Read(edtPictureFileName.Text, Bytes);
    TempStream := TBytesStream.Create(Bytes);
    try
      imgPicture.Bitmap.LoadFromStream(TempStream);
    finally
      TempStream.Free;
    end;
  finally
    ZipFile.Free;
  end;
end;

procedure TfrmInMemZip.btnSaveToFileClick(Sender: TObject);
var
  FileStream: TFileStream;
begin
  if not dlgSave.Execute then Exit;
  FileStream := TFileStream.Create(dlgSave.FileName, fmCreate);
  try
    SaveToZipStream(FileStream);
  finally
    FileStream.Free;
  end;
  MessageDlg('Created ' + dlgSave.FileName, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
end;

procedure TfrmInMemZip.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.

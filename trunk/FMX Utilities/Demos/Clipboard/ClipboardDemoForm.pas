unit ClipboardDemoForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo,
  CCR.FMXClipboard, CCR.FMXNativeDlgs;

type
  TfrmClipboardDemo = class(TForm)
    ImageControl1: TImageControl;
    btnCopyImage: TButton;
    btnPasteImage: TButton;
    Memo1: TMemo;
    btnCopyText: TButton;
    btnPasteText: TButton;
    btnPasteCustom: TButton;
    btnCopyCustom: TButton;
    btnClearImage: TButton;
    btnClearText: TButton;
    btnClose: TButton;
    btnCopyImageAndText: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnCopyImageClick(Sender: TObject);
    procedure btnPasteImageClick(Sender: TObject);
    procedure btnCopyTextClick(Sender: TObject);
    procedure btnPasteTextClick(Sender: TObject);
    procedure btnClearTextClick(Sender: TObject);
    procedure btnClearImageClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnCopyCustomClick(Sender: TObject);
    procedure btnPasteCustomClick(Sender: TObject);
    procedure btnCopyImageAndTextClick(Sender: TObject);
  private
    FCustomFormat: TClipboardFormat;
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
  end;

var
  frmClipboardDemo: TfrmClipboardDemo;

implementation

{$R *.fmx}

procedure ShowInfo(const Msg: string); inline;
begin
  MessageDlg(Msg, TMsgDlgType.mtInformation,[TMsgDlgBtn.mbOK], 0);
end;

procedure TfrmClipboardDemo.FormCreate(Sender: TObject);
begin
  FCustomFormat := Clipboard.RegisterFormat('My custom clipboard format');
  Application.OnIdle := ApplicationIdle;
end;

procedure TfrmClipboardDemo.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  btnPasteImage.Enabled := Clipboard.HasFormat(cfBitmap);
  btnPasteText.Enabled := Clipboard.HasFormat(cfText);
  btnPasteCustom.Enabled := Clipboard.HasFormat(FCustomFormat);
end;

procedure TfrmClipboardDemo.btnClearImageClick(Sender: TObject);
begin
  ImageControl1.Bitmap.Clear(0);
  btnCopyImage.Enabled := False;
end;

procedure TfrmClipboardDemo.btnClearTextClick(Sender: TObject);
begin
  Memo1.Text := '';
end;

procedure TfrmClipboardDemo.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmClipboardDemo.btnCopyCustomClick(Sender: TObject);
var
  Bytes: TBytes;
  Len: Int32;
  SeekPtr: PByte;
  Stream: TMemoryStream;
  Text: string;

  procedure CopyData(const Buffer; Size: Integer);
  begin
    Move(Buffer, SeekPtr^, Size);
    Inc(SeekPtr, Size);
  end;
begin
  Text := Memo1.Text;
  Len := Length(Text);
  Stream := TMemoryStream.Create;
  try
    ImageControl1.Bitmap.SaveToStream(Stream);
    SetLength(Bytes, SizeOf(Len) + Len + Stream.Size);
    SeekPtr := @Bytes[0];
    CopyData(Len, SizeOf(Len));
    if Len > 0 then CopyData(Text[1], Len * SizeOf(Char));
    CopyData(Stream.Memory^, Stream.Size);
  finally
    Stream.Free;
  end;
  Clipboard.Assign(FCustomFormat, Bytes);
  ShowInfo('Copied data in custom format to clipboard');
end;

procedure TfrmClipboardDemo.btnCopyImageAndTextClick(Sender: TObject);
begin
  Clipboard.Open;
  try
    Clipboard.AsText := Memo1.Text;
    Clipboard.Assign(ImageControl1.Bitmap);
  finally
    Clipboard.Close;
  end;
  ShowInfo('Copied image and text to clipboard');
end;

procedure TfrmClipboardDemo.btnCopyImageClick(Sender: TObject);
begin
  Clipboard.Assign(ImageControl1.Bitmap);
  ShowInfo('Copied image to clipboard');
end;

procedure TfrmClipboardDemo.btnCopyTextClick(Sender: TObject);
begin
  Clipboard.AsText := Memo1.Text;
  ShowInfo('Copied text to clipboard');
end;

procedure TfrmClipboardDemo.btnPasteCustomClick(Sender: TObject);
var
  Bytes: TBytes;
  Len: Int32;
  SeekPtr: PByte;
  Stream: TMemoryStream;
  Text: string;

  procedure CopyData(var Buffer; Size: Integer);
  begin
    Move(SeekPtr^, Buffer, Size);
    Inc(SeekPtr, Size);
  end;
begin
  Bytes := Clipboard.ToBytes(FCustomFormat);
  SeekPtr := @Bytes[0];
  CopyData(Len, SizeOf(Len));
  SetLength(Text, Len);
  if Len > 0 then CopyData(Text[1], Len * SizeOf(Char));
  Memo1.Text := Text;
  Stream := TMemoryStream.Create;
  try
    Stream.Size := Length(Bytes) - (SeekPtr - PByte(@Bytes[0]));
    CopyData(Stream.Memory^, Stream.Size);
    ImageControl1.Bitmap.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TfrmClipboardDemo.btnPasteImageClick(Sender: TObject);
begin
  ImageControl1.Bitmap.Assign(Clipboard);
  btnCopyImage.Enabled := True;
end;

procedure TfrmClipboardDemo.btnPasteTextClick(Sender: TObject);
begin
  Memo1.Text := Clipboard.AsText;
end;

end.

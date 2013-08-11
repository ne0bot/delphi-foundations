unit ClipboardDemoFormMobile;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, CCR.FMXClipboard;

type
  TfrmClipboardDemo = class(TForm)
    Memo1: TMemo;
    btnCopyText: TButton;
    btnPasteText: TButton;
    Image1: TImage;
    btnCopyImage: TButton;
    btnPasteImage: TButton;
    btnListFormats: TButton;
    btnCopyImageAndText: TButton;
    btnCopyCustom: TButton;
    btnPasteCustom: TButton;
    procedure btnCopyTextClick(Sender: TObject);
    procedure btnPasteTextClick(Sender: TObject);
    procedure btnCopyImageClick(Sender: TObject);
    procedure btnPasteImageClick(Sender: TObject);
    procedure btnListFormatsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCopyImageAndTextClick(Sender: TObject);
    procedure btnCopyCustomClick(Sender: TObject);
    procedure btnPasteCustomClick(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private
    FCustomFormat: TClipboardFormat;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
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
  FCustomFormat := Clipboard.RegisterFormat('com.fmxclipboard.demo');
  Application.OnIdle := AppIdle;
end;

procedure TfrmClipboardDemo.Image1Click(Sender: TObject);
begin
  if MessageDlg('Clear image?', TMsgDlgType.mtConfirmation, mbYesNo, 0) = mrYes then
    Image1.Bitmap.Clear(0);
end;

procedure TfrmClipboardDemo.AppIdle(Sender: TObject; var Done: Boolean);
begin
  btnPasteText.Enabled := Clipboard.HasFormat(cfText);
  btnPasteImage.Enabled := Clipboard.HasFormat(cfBitmap);
  btnPasteCustom.Enabled := Clipboard.HasFormat(FCustomFormat);
end;

procedure TfrmClipboardDemo.btnCopyTextClick(Sender: TObject);
begin
  Clipboard.AsText := Memo1.Text;
  ShowInfo('Copied text to clipboard');
end;

procedure TfrmClipboardDemo.btnPasteTextClick(Sender: TObject);
begin
  Memo1.Text := Clipboard.AsText;
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
  Len := Text.Length;
  Stream := TMemoryStream.Create;
  try
    Image1.Bitmap.SaveToStream(Stream);
    SetLength(Bytes, SizeOf(Len) + Len + Stream.Size);
    SeekPtr := @Bytes[0];
    CopyData(Len, SizeOf(Len));
    if Len > 0 then CopyData(Text[0], Len * SizeOf(Char));
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
    Clipboard.Assign(Image1.Bitmap);
    Clipboard.AsText := Memo1.Text;
  finally
    Clipboard.Close;
  end;
  ShowInfo('Copied image and text to clipboard');
end;

procedure TfrmClipboardDemo.btnCopyImageClick(Sender: TObject);
begin
  Clipboard.Assign(Image1.Bitmap);
  ShowInfo('Copied image to clipboard');
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
  if Len > 0 then CopyData(Text[0], Len * SizeOf(Char));
  Memo1.Text := Text;
  Stream := TMemoryStream.Create;
  try
    Stream.Size := Length(Bytes) - (SeekPtr - PByte(@Bytes[0]));
    CopyData(Stream.Memory^, Stream.Size);
    Image1.Bitmap.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TfrmClipboardDemo.btnPasteImageClick(Sender: TObject);
begin
  Image1.Bitmap.Assign(Clipboard);
end;

procedure TfrmClipboardDemo.btnListFormatsClick(Sender: TObject);
var
  Format: TClipboardFormat;
  S: string;
begin
  for Format in Clipboard.GetFormats do
    S := S + Clipboard.GetFormatName(Format) + sLineBreak;
  ShowMessage(S);
end;

end.

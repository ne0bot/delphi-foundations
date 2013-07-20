unit MacPrefsDemoForm;
{
  Simple demo of my TMacPreferencesIniFile class. Uses TMemIniFile instead when
  targeting Windows. NB - you will have to manually delete the entry for
  FMX.StdCtrls on line 11 below to compile in XE3.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.IniFiles,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.StdCtrls;

type
  TfrmMacPrefsDemo = class(TForm)
    edtSection: TEdit;
    Label1: TLabel;
    edtKey: TEdit;
    Label2: TLabel;
    edtValue: TEdit;
    Label3: TLabel;
    btnWriteAsBoolean: TButton;
    btnWriteAsFloat: TButton;
    btnWriteAsInt: TButton;
    btnWriteAsStr: TButton;
    btnUpdateFile: TButton;
    btnReadAsBool: TButton;
    btnReadAsFloat: TButton;
    btnReadAsInt: TButton;
    btnReadAsStr: TButton;
    ImageControl1: TImageControl;
    btnImageRoundtrip: TButton;
    btnOpenFile: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnWriteAsBooleanClick(Sender: TObject);
    procedure btnWriteAsFloatClick(Sender: TObject);
    procedure btnWriteAsIntClick(Sender: TObject);
    procedure btnWriteAsStrClick(Sender: TObject);
    procedure btnReadAsBoolClick(Sender: TObject);
    procedure btnReadAsFloatClick(Sender: TObject);
    procedure btnReadAsIntClick(Sender: TObject);
    procedure btnReadAsStrClick(Sender: TObject);
    procedure btnUpdateFileClick(Sender: TObject);
    procedure btnImageRoundtripClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
  private
    FIniFile: TCustomIniFile;
  end;

var
  frmMacPrefsDemo: TfrmMacPrefsDemo;

implementation

uses
  CCR.MacPrefsIniFile, ShellUtils;

{$R *.fmx}

procedure TfrmMacPrefsDemo.FormCreate(Sender: TObject);
begin
  FIniFile := CreateUserPreferencesIniFile(TWinLocation.IniFile);
end;

procedure TfrmMacPrefsDemo.FormDestroy(Sender: TObject);
begin
  FIniFile.Free;
end;

procedure TfrmMacPrefsDemo.btnImageRoundtripClick(Sender: TObject);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    ImageControl1.Bitmap.SaveToStream(Stream);
    Stream.Position := 0;
    FIniFile.WriteBinaryStream('Images', 'Coffee cup', Stream);
    Stream.Clear;
    ImageControl1.Bitmap.Clear(0);
    ShowMessage('Ready to reload...');
    FIniFile.ReadBinaryStream('Images', 'Coffee cup', Stream);
    Stream.Position := 0;
    ImageControl1.Bitmap.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TfrmMacPrefsDemo.btnWriteAsBooleanClick(Sender: TObject);
begin
  FIniFile.WriteBool(edtSection.Text, edtKey.Text, StrToBool(edtValue.Text));
end;

procedure TfrmMacPrefsDemo.btnWriteAsFloatClick(Sender: TObject);
begin
  FIniFile.WriteFloat(edtSection.Text, edtKey.Text, StrToFloat(edtValue.Text));
end;

procedure TfrmMacPrefsDemo.btnWriteAsIntClick(Sender: TObject);
begin
  FIniFile.WriteInteger(edtSection.Text, edtKey.Text, StrToInt(edtValue.Text));
end;

procedure TfrmMacPrefsDemo.btnWriteAsStrClick(Sender: TObject);
begin
  FIniFile.WriteString(edtSection.Text, edtKey.Text, edtValue.Text);
end;

procedure TfrmMacPrefsDemo.btnUpdateFileClick(Sender: TObject);
begin
  FIniFile.UpdateFile;
end;

procedure TfrmMacPrefsDemo.btnOpenFileClick(Sender: TObject);
begin
  if FileExists(FIniFile.FileName) then
    ShellOpen(FIniFile.FileName)
  else
    MessageDlg('Preferences file does not exist yet - try clicking "Flush Changes to Disk" first.',
      TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
end;

procedure TfrmMacPrefsDemo.btnReadAsBoolClick(Sender: TObject);
begin
  edtValue.Text := BoolToStr(FIniFile.ReadBool(edtSection.Text, edtKey.Text, False), True)
end;

procedure TfrmMacPrefsDemo.btnReadAsFloatClick(Sender: TObject);
begin
  edtValue.Text := FloatToStr(FIniFile.ReadFloat(edtSection.Text, edtKey.Text, 0))
end;

procedure TfrmMacPrefsDemo.btnReadAsIntClick(Sender: TObject);
begin
  edtValue.Text := IntToStr(FIniFile.ReadInteger(edtSection.Text, edtKey.Text, 0))
end;

procedure TfrmMacPrefsDemo.btnReadAsStrClick(Sender: TObject);
begin
  edtValue.Text := FIniFile.ReadString(edtSection.Text, edtKey.Text, '')
end;

end.

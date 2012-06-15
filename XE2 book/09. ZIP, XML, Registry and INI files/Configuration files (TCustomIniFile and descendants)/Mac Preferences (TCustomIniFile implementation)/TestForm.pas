unit TestForm;
{
  Simple demo of my TMacPreferencesIniFile class. Uses TMemIniFile instead when
  targeting Windows.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, System.IniFiles;

type
  TForm1 = class(TForm)
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
  Form1: TForm1;

implementation

{$IFDEF MSWINDOWS}
uses WinApi.Windows, WinApi.ShellApi;
{$ENDIF}
{$IFDEF MACOS}
uses Posix.StdLib, System.StrUtils, CCR.MacPrefsIniFile;
{$ENDIF}

{$R *.fmx}

procedure ShellOpen(const FileName: string);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, nil, PChar(FileName), nil, nil, SW_SHOWNORMAL)
{$ELSE}
  _system(PAnsiChar(UTF8String('open "' +
    ReplaceStr(ReplaceStr(FileName, '\', '\\'), '"', '\"') + '"')));
{$ENDIF};
end;

procedure TForm1.FormCreate(Sender: TObject);
{$IFDEF MACOS}
begin
  FIniFile := TMacPreferencesIniFile.Create;
end;
{$ELSE}
var
  Dir: string;
begin
  Dir := IncludeTrailingPathDelimiter(GetHomePath) + 'Preferences Test'; //e.g. C:\Users\CCR\AppData\Roaming\Preferences Test
  ForceDirectories(Dir);
  FIniFile := TMemIniFile.Create(Dir + PathDelim + 'Settings.ini', TEncoding.UTF8);
end;
{$ENDIF}

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FIniFile.Free;
end;

procedure TForm1.btnImageRoundtripClick(Sender: TObject);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    ImageControl1.Bitmap.SaveToStream(Stream);
    Stream.Position := 0;
    FIniFile.WriteBinaryStream('Images', 'Coffee cup', Stream);
    Stream.Clear;
    ImageControl1.Bitmap.Clear(claAqua);
    ShowMessage('Ready to reload...');
    FIniFile.ReadBinaryStream('Images', 'Coffee cup', Stream);
    Stream.Position := 0;
    ImageControl1.Bitmap.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TForm1.btnWriteAsBooleanClick(Sender: TObject);
begin
  FIniFile.WriteBool(edtSection.Text, edtKey.Text, StrToBool(edtValue.Text));
end;

procedure TForm1.btnWriteAsFloatClick(Sender: TObject);
begin
  FIniFile.WriteFloat(edtSection.Text, edtKey.Text, StrToFloat(edtValue.Text));
end;

procedure TForm1.btnWriteAsIntClick(Sender: TObject);
begin
  FIniFile.WriteInteger(edtSection.Text, edtKey.Text, StrToInt(edtValue.Text));
end;

procedure TForm1.btnWriteAsStrClick(Sender: TObject);
begin
  FIniFile.WriteString(edtSection.Text, edtKey.Text, edtValue.Text);
end;

procedure TForm1.btnUpdateFileClick(Sender: TObject);
begin
  FIniFile.UpdateFile;
end;

procedure TForm1.btnOpenFileClick(Sender: TObject);
begin
  ShellOpen(FIniFile.FileName);
end;

procedure TForm1.btnReadAsBoolClick(Sender: TObject);
begin
  edtValue.Text := BoolToStr(FIniFile.ReadBool(edtSection.Text, edtKey.Text, False), True)
end;

procedure TForm1.btnReadAsFloatClick(Sender: TObject);
begin
  edtValue.Text := FloatToStr(FIniFile.ReadFloat(edtSection.Text, edtKey.Text, 0))
end;

procedure TForm1.btnReadAsIntClick(Sender: TObject);
begin
  edtValue.Text := IntToStr(FIniFile.ReadInteger(edtSection.Text, edtKey.Text, 0))
end;

procedure TForm1.btnReadAsStrClick(Sender: TObject);
begin
  edtValue.Text := FIniFile.ReadString(edtSection.Text, edtKey.Text, '')
end;

end.

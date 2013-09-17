unit AndroidPrefsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, CCR.PrefsIniFile.Android,
  FMX.StdCtrls, FMX.Edit;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    edtSection: TEdit;
    Label2: TLabel;
    edtKey: TEdit;
    edtValue: TEdit;
    Label3: TLabel;
    btnReadBool: TButton;
    btnReadFloat: TButton;
    btnReadInt: TButton;
    btnReadStr: TButton;
    btnWriteBool: TButton;
    btnWriteFloat: TButton;
    btnWriteInt: TButton;
    btnWriteStr: TButton;
    btnUpdateFile: TButton;
    btnReadSection: TButton;
    btnReadSectionVals: TButton;
    btnReadSections: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnReadBoolClick(Sender: TObject);
    procedure btnReadFloatClick(Sender: TObject);
    procedure btnReadIntClick(Sender: TObject);
    procedure btnReadStrClick(Sender: TObject);
    procedure btnWriteBoolClick(Sender: TObject);
    procedure btnWriteFloatClick(Sender: TObject);
    procedure btnWriteIntClick(Sender: TObject);
    procedure btnWriteStrClick(Sender: TObject);
    procedure btnUpdateFileClick(Sender: TObject);
    procedure btnReadSectionClick(Sender: TObject);
    procedure btnReadSectionValsClick(Sender: TObject);
    procedure btnReadSectionsClick(Sender: TObject);
  private
    FIniFile: TAndroidPreferencesIniFile;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btnReadBoolClick(Sender: TObject);
begin
  edtValue.Text := BoolToStr(FIniFile.ReadBool(edtSection.Text, edtKey.Text, StrToBoolDef(edtValue.Text, False)), True);
end;

procedure TForm1.btnReadFloatClick(Sender: TObject);
begin
  edtValue.Text := FIniFile.ReadFloat(edtSection.Text, edtKey.Text, StrToFloatDef(edtValue.Text, 0)).ToString;
end;

procedure TForm1.btnReadIntClick(Sender: TObject);
begin
  edtValue.Text := FIniFile.ReadInteger(edtSection.Text, edtKey.Text, StrToIntDef(edtValue.Text, 0)).ToString;
end;

procedure TForm1.btnReadSectionClick(Sender: TObject);
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  FIniFile.ReadSection(edtSection.Text, Strings);
  ShowMessage(Strings.Text);
end;

procedure TForm1.btnReadSectionsClick(Sender: TObject);
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  FIniFile.ReadSections(Strings);
  ShowMessage(Strings.Text);
end;

procedure TForm1.btnReadSectionValsClick(Sender: TObject);
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  FIniFile.ReadSectionValues(edtSection.Text, Strings);
  ShowMessage(Strings.Text);
end;

procedure TForm1.btnReadStrClick(Sender: TObject);
begin
  edtValue.Text := FIniFile.ReadString(edtSection.Text, edtKey.Text, edtValue.Text);
end;

procedure TForm1.btnWriteBoolClick(Sender: TObject);
begin
  FIniFile.WriteBool(edtSection.Text, edtKey.Text, StrToBoolDef(edtValue.Text, False));
end;

procedure TForm1.btnWriteFloatClick(Sender: TObject);
begin
  FIniFile.WriteFloat(edtSection.Text, edtKey.Text, StrToFloatDef(edtValue.Text, 0));
end;

procedure TForm1.btnWriteIntClick(Sender: TObject);
begin
  FIniFile.WriteInteger(edtSection.Text, edtKey.Text, StrToIntDef(edtValue.Text, 0));
end;

procedure TForm1.btnWriteStrClick(Sender: TObject);
begin
  FIniFile.WriteString(edtSection.Text, edtKey.Text, edtValue.Text);
end;

procedure TForm1.btnUpdateFileClick(Sender: TObject);
begin
  FIniFile.UpdateFile;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FIniFile := TAndroidPreferencesIniFile.Create;
end;

end.

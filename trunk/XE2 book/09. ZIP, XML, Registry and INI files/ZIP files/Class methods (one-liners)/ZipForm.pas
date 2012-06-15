unit ZipForm;
{
  Demo of the class methods on TZipFile. For greater flexibility, you should
  instantiate TZipFile and use its instance members, which is all the class
  methods do internally.
}
interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmZipClassMethods = class(TForm)
    btnIsValid: TButton;
    dlgOpenZip: TOpenDialog;
    btnExtract: TButton;
    dlgSelectDirVistaPlus: TFileOpenDialog;
    btnConvert: TButton;
    dlgSave: TSaveDialog;
    btnClose: TButton;
    procedure btnIsValidClick(Sender: TObject);
    procedure btnExtractClick(Sender: TObject);
    procedure btnConvertClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  strict private
    function ChooseDirectory(const ADlgTitle: string; var ADir: string): Boolean;
  end;

var
  frmZipClassMethods: TfrmZipClassMethods;

implementation

uses Winapi.ShellApi, System.Zip, Vcl.FileCtrl;

{$R *.dfm}

function TfrmZipClassMethods.ChooseDirectory(const ADlgTitle: string;
  var ADir: string): Boolean;
begin
  //use the Vista+ folder selection dialog if we can, otherwise use the version that dates back to Win95
  if Win32MajorVersion >= 6 then
  begin
    dlgSelectDirVistaPlus.FileName := '';
    dlgSelectDirVistaPlus.Title := ADlgTitle;
    Result := dlgSelectDirVistaPlus.Execute;
    if Result then ADir := dlgSelectDirVistaPlus.FileName; //sic.
  end
  else
    Result := SelectDirectory(ADlgTitle, '', ADir);
end;

procedure TfrmZipClassMethods.btnIsValidClick(Sender: TObject);
begin
  dlgOpenZip.Title := 'Select ZIP file to test';
  if dlgOpenZip.Execute then
    if TZipFile.IsValid(dlgOpenZip.FileName) then
      MessageDlg(dlgOpenZip.FileName + ' is a valid ZIP file', mtInformation, [mbOK], 0)
    else
      MessageDlg(dlgOpenZip.FileName + ' is NOT a valid ZIP file', mtWarning, [mbOK], 0)
end;

procedure TfrmZipClassMethods.btnExtractClick(Sender: TObject);
var
  OutputDir: string;
begin
  dlgOpenZip.Title := 'Step 1: select ZIP file to extract';
  if not dlgOpenZip.Execute then Exit;
  if not ChooseDirectory('Step 2: choose folder to extract to', OutputDir) then Exit;
  TZipFile.ExtractZipFile(dlgOpenZip.FileName, OutputDir);
  if IsPositiveResult(MessageDlg('Open destination folder in Windows Explorer?',
    mtConfirmation, mbOKCancel, 0)) then ShellExecute(Handle, nil, 'explorer.exe',
      PChar(OutputDir), nil, SW_SHOWNORMAL);
end;

procedure TfrmZipClassMethods.btnConvertClick(Sender: TObject);
var
  SourceDir: string;
begin
  if not ChooseDirectory('Step 1: choose folder to compress', SourceDir) then Exit;
  if not dlgSave.Execute then Exit;
  TZipFile.ZipDirectoryContents(dlgSave.FileName, SourceDir);
  MessageDlg('Created ZIP file', mtInformation, [mbOK], 0);
end;

procedure TfrmZipClassMethods.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.

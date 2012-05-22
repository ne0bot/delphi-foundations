unit MainForm;
{
  Demonstrates using TResourceStream to load the form's own form definition
  resource, a picture embedded in the executable, and a picture embedded in a
  resource DLL/dylib, itself created in Delphi.

  For the Mac, compile the library using the Release build configuation (since we
  define no code for it, the Debug configuration is useless), then add the
  compiled dylib to the executable's deployed files: go to Project|Deployment,
  and in the tab that results, select All Configurations - OS X Platform from
  the combo box at the top before clicking on the Add Files button (second from
  the left).
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo;

type
  TfrmMain = class(TForm)
    memFormDefinition: TMemo;
    btnLoadFormDefinition: TButton;
    imgLocal: TImageControl;
    btnLoadPictureFromExecutable: TButton;
    btnLoadPictureFromLibrary: TButton;
    imgLibrary: TImageControl;
    procedure btnLoadFormDefinitionClick(Sender: TObject);
    procedure btnLoadPictureFromExecutableClick(Sender: TObject);
    procedure btnLoadPictureFromLibraryClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$IFDEF MSWINDOWS}
uses Winapi.Windows;
{$ENDIF}

const
  LibName = {$IFDEF MSWINDOWS}'MyResources.dll'{$ELSE}'libMyResources.dylib'{$ENDIF};

{$R *.fmx}

procedure TfrmMain.btnLoadFormDefinitionClick(Sender: TObject);
var
  InStream: TResourceStream;
  OutStream: TStringStream;
begin
  OutStream := nil;
  InStream := TResourceStream.Create(HInstance, ClassName, RT_RCDATA);
  try
    OutStream := TStringStream.Create;
    ObjectBinaryToText(InStream, OutStream);
    memFormDefinition.Text := OutStream.DataString;
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

procedure TfrmMain.btnLoadPictureFromExecutableClick(Sender: TObject);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(HInstance, 'Taff', RT_RCDATA);
  try
    imgLocal.Bitmap.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TfrmMain.btnLoadPictureFromLibraryClick(Sender: TObject);
var
  LibHandle: HMODULE;
  Stream: TResourceStream;
begin
  Stream := nil;
  LibHandle := LoadLibrary(LibName);
  if LibHandle = 0 then raise EFileNotFoundException.Create('Library not found');
  try
    Stream := TResourceStream.Create(LibHandle, 'Dublin', RT_RCDATA);
    imgLibrary.Bitmap.LoadFromStream(Stream);
  finally
    Stream.Free;
    FreeLibrary(LibHandle)
  end;
end;

end.

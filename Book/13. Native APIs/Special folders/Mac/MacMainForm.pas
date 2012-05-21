unit MacMainForm;
{
  Demonstrates the use of NSFileManager to retrieve the location of special folders.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Grid, FMX.Layouts;

type
  TfrmMacSpecialFolders = class(TForm)
    stgFolders: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    StringColumn5: TStringColumn;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMacSpecialFolders: TfrmMacSpecialFolders;

implementation

uses Macapi.CoreFoundation, Macapi.ObjectiveC, Macapi.CocoaTypes, Macapi.Foundation;

{$R *.fmx}

function CFToDelphiString(const CFStr: CFStringRef): string;
var
  Range: CFRange;
begin
  Range.location := 0;
  Range.length := CFStringGetLength(CFStr);
  SetLength(Result, Range.length);
  if Range.length = 0 then Exit;
  CFStringGetCharacters(CFStr, Range, PWideChar(Result));
end;

function NSToDelphiString(const NSStr: NSString): string; inline;
begin
  Result := CFToDelphiString((NSStr as ILocalObject).GetObjectID);
end;

procedure TfrmMacSpecialFolders.FormCreate(Sender: TObject);
const
  Names: array[NSApplicationDirectory..NSPreferencePanesDirectory] of string = (
    'NSApplicationDirectory', 'NSDemoApplicationDirectory', 'NSDeveloperApplicationDirectory',
    'NSAdminApplicationDirectory', 'NSLibraryDirectory', 'NSDeveloperDirectory',
    'NSUserDirectory', 'NSDocumentationDirectory', 'NSDocumentDirectory',
    'NSCoreServicesDirectory', 'NSAutosavedInformationDirectory', 'NSDesktopDirectory',
    'NSCachesDirectory', 'NSApplicationSupportDirectory', 'NSDownloadsDirectory',
    'NSInputMethodsDirectory', 'NSMoviesDirectory', 'NSMusicDirectory', 'NSPicturesDirectory',
    'NSPrinterDescriptionDirectory', 'NSSharedPublicDirectory', 'NSPreferencePanesDirectory');
var
  DirectoryID: Integer;
  Manager: NSFileManager;

  function GetDir(Domain: NSSearchPathDomainMask): string;
  var
    URL: NSURL;
  begin
    URL := Manager.URLForDirectory(DirectoryID, Domain, nil, False, nil);
    if URL <> nil then
      Result := NSToDelphiString(URL.path)
    else
      Result := 'n/a';
  end;
begin
  stgFolders.RowCount := High(Names) - Low(Names) + 1;
  Manager := TNSFileManager.Wrap(TNSFileManager.OCClass.defaultManager);
  for DirectoryID := Low(Names) to High(Names) do
  begin
    stgFolders.Cells[0, DirectoryID - Low(Names)] := Names[DirectoryID];
    stgFolders.Cells[1, DirectoryID - Low(Names)] := GetDir(NSUserDomainMask);
    stgFolders.Cells[2, DirectoryID - Low(Names)] := GetDir(NSLocalDomainMask);
    stgFolders.Cells[3, DirectoryID - Low(Names)] := GetDir(NSNetworkDomainMask);
    stgFolders.Cells[4, DirectoryID - Low(Names)] := GetDir(NSSystemDomainMask);
  end;
end;

end.

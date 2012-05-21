unit WinMainForm;
{
  Demonstrates the use of SHGetSpecialFolderPath to retrieve the location of special folders.
}
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls;

type
  TfrmWinSpecialFolders = class(TForm)
    lsvFolders: TListView;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmWinSpecialFolders: TfrmWinSpecialFolders;

implementation

uses Winapi.ShlObj;

{$R *.dfm}

procedure TfrmWinSpecialFolders.FormCreate(Sender: TObject);
const
  Names: array[CSIDL_DESKTOP..CSIDL_CDBURN_AREA] of string = ('CSIDL_DESKTOP',
    'CSIDL_INTERNET', 'CSIDL_PROGRAMS', 'CSIDL_CONTROLS', 'CSIDL_PRINTERS',
    'CSIDL_MYDOCUMENTS', 'CSIDL_FAVORITES', 'CSIDL_STARTUP', 'CSIDL_RECENT',
    'CSIDL_SENDTO', 'CSIDL_BITBUCKET', 'CSIDL_STARTMENU', 'CSIDL_$000C',
    'CSIDL_MYMUSIC', 'CSIDL_MYVIDEO', '', 'CSIDL_DESKTOPDIRECTORY',
    'CSIDL_DRIVES', 'CSIDL_NETWORK', 'CSIDL_NETHOOD', 'CSIDL_FONTS' ,
    'CSIDL_TEMPLATES', 'CSIDL_COMMON_STARTMENU', 'CSIDL_COMMON_PROGRAMS',
    'CSIDL_COMMON_STARTUP', 'CSIDL_COMMON_DESKTOPDIRECTORY', 'CSIDL_APPDATA',
    'CSIDL_PRINTHOOD', 'CSIDL_LOCAL_APPDATA', 'CSIDL_ALTSTARTUP',
    'CSIDL_COMMON_ALTSTARTUP', 'CSIDL_COMMON_FAVORITES', 'CSIDL_INTERNET_CACHE',
    'CSIDL_COOKIES', 'CSIDL_HISTORY', 'CSIDL_COMMON_APPDATA', 'CSIDL_WINDOWS',
    'CSIDL_SYSTEM', 'CSIDL_PROGRAM_FILES', 'CSIDL_MYPICTURES', 'CSIDL_PROFILE',
    'CSIDL_SYSTEMX86', 'CSIDL_PROGRAM_FILESX86', 'CSIDL_PROGRAM_FILES_COMMON',
    'CSIDL_PROGRAM_FILES_COMMONX86', 'CSIDL_COMMON_TEMPLATES',
    'CSIDL_COMMON_DOCUMENTS', 'CSIDL_COMMON_ADMINTOOLS', 'CSIDL_ADMINTOOLS',
    'CSIDL_CONNECTIONS', '', '', '', 'CSIDL_COMMON_MUSIC', 'CSIDL_COMMON_PICTURES',
    'CSIDL_COMMON_VIDEO', 'CSIDL_RESOURCES', 'CSIDL_RESOURCES_LOCALIZED',
    'CSIDL_COMMON_OEM_LINKS', 'CSIDL_CDBURN_AREA');
var
  Buffer: array[0..MAX_PATH] of Char;
  Dir: string;
  ID: Integer;
  ListItem: TListItem;
begin
  for ID := Low(Names) to High(Names) do
    if Names[ID] <> '' then
    begin
      if SHGetSpecialFolderPath(0, Buffer, ID, False) then
        Dir := Buffer
      else
        Dir := 'n/a';
      ListItem := lsvFolders.Items.Add;
      ListItem.Caption := Names[ID];
      ListItem.SubItems.Add(Dir)
    end;
end;

end.

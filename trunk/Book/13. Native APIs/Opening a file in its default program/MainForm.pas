unit MainForm;

interface

uses
  System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs;

type
  TfrmShellOpen = class(TForm)
    btnSelect: TButton;
    dlgOpen: TOpenDialog;
    btnClose: TButton;
    procedure btnSelectClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  end;

var
  frmShellOpen: TfrmShellOpen;

implementation

{$R *.fmx}

uses
  {$IF DEFINED(MSWINDOWS)}
  WinApi.Windows, WinApi.ShellApi,
  {$ELSEIF DEFINED(MACOS)}
  Posix.Stdlib, Macapi.CoreFoundation, Macapi.Foundation, Macapi.AppKit,
  {$IFEND}
  System.SysUtils, System.StrUtils, System.Masks;

procedure ShellOpen(const FileName: string);
begin
{$IF DEFINED(MSWINDOWS)}
  ShellExecute(0, nil, PChar(FileName), nil, nil, SW_SHOWNORMAL)
{$ELSEIF DEFINED(MACOS)}
  _system(PAnsiChar(UTF8String('open "' +
    ReplaceStr(ReplaceStr(FileName, '\', '\\'), '"', '\"') + '"')));
{$ELSE}
  {$MESSAGE ERROR 'Not yet implemented'}
{$IFEND};
end;

{$IFDEF MACOS}
procedure NSShellOpen(const FileName: string); //alternative implementation for OS X that uses the Cocoa rather than the Posix layer
var
  MacStr: NSString;
  URL: NSURL;
  Workspace: NSWorkspace;
begin
  MacStr := TNSString.Wrap(CFStringCreateWithCharactersNoCopy(nil,
    PWideChar(FileName), Length(FileName), kCFAllocatorNull));
  Workspace := TNSWorkspace.Wrap(TNSWorkspace.OCClass.sharedWorkspace);
  if StartsText('http://', FileName) or StartsText('mailto:', FileName) then
  begin
    URL := TNSURL.Wrap(TNSURL.OCClass.URLWithString(MacStr));
    Workspace.openURL(URL);
  end
  else
    Workspace.openFile(MacStr);
  MacStr.Release;
end;
{$ENDIF}

procedure TfrmShellOpen.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmShellOpen.btnSelectClick(Sender: TObject);
begin
  if dlgOpen.Execute then ShellOpen(dlgOpen.FileName);
end;

end.

unit CustomNSApplicationDelegate;
{
  Implements a custom NSApplication delegate which traps application:openFile: messages.
}
interface

uses
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.AppKit;

type
  { While Objective-C protocols roughly correspond to Delphi interfaces, a difference is
    that the former frequently include members that you don't have to actually implement.
    As they are mapped to interface types in the Macapi.* units, protocols are therefore
    declared with the minimum number of methods. Since in our case we want to handle one
    more message than what the FMX default NSApplication delegate does, we must derive a
    new interface type with the corresponding additional method on it. }
  NSApplicationDelegate2 = interface(NSApplicationDelegate)
    ['{BE9AEDB7-80AC-49B1-8921-F226CC9310F4}']
    function application(theApplication: Pointer; openFile: CFStringRef): Boolean; cdecl;
  end;

  TOpenFileEvent = reference to procedure (const AFileName: string);

  TNSApplicationDelegate2 = class(TOCLocal, NSApplicationDelegate2)
  private
    FOnOpenFile: TOpenFileEvent;
  public
    constructor Create(const AOnOpenFile: TOpenFileEvent);
    procedure applicationDidFinishLaunching(Notification: Pointer); cdecl;
    procedure applicationWillTerminate(Notification: Pointer); cdecl;
    function application(theApplication: Pointer; openFile: CFStringRef): Boolean; cdecl;
  end;

procedure InstallApplicationDelegate2(const AOnOpenFile: TOpenFileEvent);

implementation

uses FMX.Forms;

var
  Delegate: NSApplicationDelegate2;

procedure InstallApplicationDelegate2(const AOnOpenFile: TOpenFileEvent);
var
  NSApp: NSApplication;
begin
  Assert(Delegate = nil);
  NSApp := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
  Delegate := TNSApplicationDelegate2.Create(AOnOpenFile);
  NSApp.setDelegate(Delegate);
end;

constructor TNSApplicationDelegate2.Create(const AOnOpenFile: TOpenFileEvent);
begin
  inherited Create;
  FOnOpenFile := AOnOpenFile;
end;

function TNSApplicationDelegate2.application(theApplication: Pointer;
  openFile: CFStringRef): Boolean;
var
  Range: CFRange;
  S: string;
begin
  Result := Assigned(FOnOpenFile);
  if not Result then Exit;
  Range.location := 0;
  Range.length := CFStringGetLength(openFile);
  SetLength(S, Range.length);
  CFStringGetCharacters(openFile, Range, PChar(S));
  try
    FOnOpenFile(S);
  except
    FMX.Forms.Application.HandleException(ExceptObject);
    Result := False;
  end;
end;

procedure TNSApplicationDelegate2.applicationDidFinishLaunching(
  Notification: Pointer);
begin
  { The default FMX delegate doesn't do anything here, so nor will we. }
end;

procedure TNSApplicationDelegate2.applicationWillTerminate(
  Notification: Pointer);
begin
  { Do NOT do a FreeAndNil, because the Application global is referenced deep in
    the bowels of a form's destruction code, and freeing the Application object
    causes the main form to be freed. }
  FMX.Forms.Application.Free;
  FMX.Forms.Application := nil;
end;

end.

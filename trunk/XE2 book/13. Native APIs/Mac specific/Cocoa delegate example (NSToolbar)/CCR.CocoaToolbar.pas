unit CCR.CocoaToolbar;
{
  Encapsulates the code to set up an NSToolbar.

  This demo has a sibling relationship to the NSAlert one, which is worked
  though in the book. If you have downloaded this code independently, the book
  in question is Delphi XE2 Foundations by me (Chris Rolliston). The book is
  available now on Amazon in both printed and Kindle eBook versions; for more
  info, see http://delphifoundations.com
}
interface

uses
  System.SysUtils, System.Classes, System.TypInfo, System.Generics.Collections,
  Macapi.ObjCRuntime, Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes,
  Macapi.Foundation, Macapi.AppKit, FMX.Forms;

type
  TNSToolbarHelperItem = class;

  TNSToolbarUpdateEvent = reference to procedure (Sender: TNSToolbarHelperItem;
    var EnableItem: Boolean);

  TNSToolbarHelperItem = class
  strict private
    FCachedCocoaObject: NSToolbarItem;
    FID: string;
    FShowByDefault: Boolean;
    FOnClick: TProc<TObject>;
    FOnUpdate: TNSToolbarUpdateEvent;
    function GetCocoaObject: NSToolbarItem;
  private
    constructor Create(const AID: string; AShowByDefault: Boolean);
  public
    destructor Destroy; override;
    procedure SetCaptionImageAndHint(const ACaption, AImageFileName, AHint: string);
    property CocoaObject: NSToolbarItem read GetCocoaObject;
    property ID: string read FID;
    property ShowByDefault: Boolean read FShowByDefault;
    property OnClick: TProc<TObject> read FOnClick write FOnClick;
    property OnUpdate: TNSToolbarUpdateEvent read FOnUpdate write FOnUpdate;
  end;

  TNSToolbarHelper = class
  strict private
    FAttachedForm: TCommonCustomForm;
    FDefaultItemCount: Integer;
    FDelegate: NSToolbarDelegate;
    FItems: TObjectList<TNSToolbarHelperItem>;
    FItemMap: TDictionary<string, TNSToolbarHelperItem>;
    FSpacerCount: Integer;
    FToolbar: NSToolbar;
    procedure DoAddItem(const AIDForMap: string; AItem: TNSToolbarHelperItem);
    function GetItem(const ItemID: string): TNSToolbarHelperItem;
  protected
    function CreateNSArrayOfItemIDs(ADefaultOnly: Boolean): NSArray;
  public
    constructor Create(const AToolbarID: string);
    destructor Destroy; override;
    function AddItem(const AItemID: string; AShowByDefault: Boolean = True): TNSToolbarHelperItem;
    procedure AddFlexibleSpaceItem(AShowByDefault: Boolean = True);
    procedure AddSpaceItem(AShowByDefault: Boolean = True);
    procedure Attach(AForm: TCommonCustomForm);
    property Items[const ItemID: string]: TNSToolbarHelperItem read GetItem; default;
    property Toolbar: NSToolbar read FToolbar;
  end;

implementation

uses FMX.Platform.Mac;

{$M+}
type
  { This is the interface for our custom Objective-C class... }
  INSToolbarDelegate = interface(NSObject)
    ['{17797346-6D03-46C1-982D-6840549F78BD}']
    //required methods
    function toolbarAllowedItemIdentifiers(toolbar: Pointer): NSArray; cdecl;
    function toolbarDefaultItemIdentifiers(toolbar: Pointer): NSArray; cdecl;
    function toolbar(toolbar: Pointer; itemForItemIdentifier: CFStringRef;
      willBeInsertedIntoToolbar: Boolean): NSToolbarItem; cdecl;
    //optional method
    function validateToolbarItem(theItem: NSToolbarItem): Boolean; cdecl;
    //custom method
    procedure ItemClicked(Sender: NSToolbarItem); cdecl;
  end;

  { ... and this is the implementation. Note the interface representing the
    Objective-C class is *not* formally implemented by the Delph one, being
    indicated instead by the return value of GetObjectiveCClass. }
  TNSToolbarDelegate = class(TOCLocal, NSToolbarDelegate)
  private
    FClickEventSelector: SEL;
    FOwner: TNSToolbarHelper;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create(AOwner: TNSToolbarHelper); reintroduce;
    destructor Destroy; override;
    function toolbarAllowedItemIdentifiers(toolbar: Pointer): NSArray; cdecl;
    function toolbarDefaultItemIdentifiers(toolbar: Pointer): NSArray; cdecl;
    function toolbar(toolbar: Pointer; itemForItemIdentifier: CFStringRef;
      willBeInsertedIntoToolbar: Boolean): NSToolbarItem; cdecl;
    function validateToolbarItem(theItem: NSToolbarItem): Boolean; cdecl;
    procedure ItemClicked(Sender: NSToolbarItem); cdecl;
  end;
{$M-}

  TOCLocalAccess = class(TOCLocal);

function NSWindowOfForm(Form: TCommonCustomForm): NSWindow;
var
  Obj: TOCLocal;
begin
  Obj := (FmxHandleToObjC(Form.Handle) as TOCLocal);
  Result := NSWindow(TOCLocalAccess(Obj).Super);
end;

function GetDelphiFromCFString(const CF: CFStringRef): string;
var
  Range: CFRange;
begin
  Range.location := 0;
  Range.length := CFStringGetLength(CF);
  SetLength(Result, Range.length);
  CFStringGetCharacters(CF, Range, PChar(Result));
end;

function GetDelphiFromNSString(const NS: NSString): string;
begin
  Result := GetDelphiFromCFString((NS as ILocalObject).GetObjectID);
end;

{ TNSToolbarHelperItem }

constructor TNSToolbarHelperItem.Create(const AID: string; AShowByDefault: Boolean);
begin
  inherited Create;
  FID := AID;
  FShowByDefault := AShowByDefault;
end;

destructor TNSToolbarHelperItem.Destroy;
begin
  if FCachedCocoaObject <> nil then FCachedCocoaObject.release;
  inherited;
end;

function TNSToolbarHelperItem.GetCocoaObject: NSToolbarItem;
var
  ObjID: Pointer;
begin
  if FCachedCocoaObject = nil then
  begin
    FCachedCocoaObject := TNSToolbarItem.Wrap(TNSToolbarItem.OCClass.alloc);
    ObjID := FCachedCocoaObject.initWithItemIdentifier(NSSTR(FID));
    TNSToolbarItem.Init(FCachedCocoaObject, ObjID);
  end;
  Result := FCachedCocoaObject;
end;

procedure TNSToolbarHelperItem.SetCaptionImageAndHint(const ACaption,
  AImageFileName, AHint: string);
var
  NSCaption: NSString;
  Obj: NSToolbarItem;
begin
  NSCaption := NSSTR(ACaption);
  Obj := GetCocoaObject;
  Obj.setLabel(NSCaption);        //caption in the toolbar
  Obj.setPaletteLabel(NSCaption); //caption in the customise dlg
  Obj.setImage(TNSImage.Wrap(TNSImage.OCClass.imageNamed(NSSTR(AImageFileName))));
  Obj.setToolTip(NSSTR(AHint));
end;

{ TNSToolbarHelper }

constructor TNSToolbarHelper.Create(const AToolbarID: string);
var
  ObjID: Pointer;
begin
  inherited Create;
  FItems := TObjectList<TNSToolbarHelperItem>.Create;
  FItemMap := TDictionary<string, TNSToolbarHelperItem>.Create;
  FDelegate := TNSToolbarDelegate.Create(Self);
  FToolbar := TNSToolbar.Wrap(TNSToolbar.OCClass.alloc);
  ObjID := FToolbar.initWithIdentifier(NSSTR(AToolbarID));
  TNSToolbar.Init(FToolbar, ObjID);
  FToolbar.setDelegate(FDelegate);
end;

destructor TNSToolbarHelper.Destroy;
begin
  Attach(nil);
  if FToolbar <> nil then FToolbar.release;
  FItemMap.Free;
  FItems.Free;
  inherited;
end;

procedure TNSToolbarHelper.DoAddItem(const AIDForMap: string; AItem: TNSToolbarHelperItem);
begin
  try
    FItemMap.Add(AIDForMap, AItem);
  except
    AItem.Free;
    raise;
  end;
  FItems.Add(AItem);
  if AItem.ShowByDefault then Inc(FDefaultItemCount);
end;

function TNSToolbarHelper.AddItem(const AItemID: string;
  AShowByDefault: Boolean): TNSToolbarHelperItem;
begin
  Result := TNSToolbarHelperItem.Create(AItemID, AShowByDefault);
  DoAddItem(AItemID, Result);
end;

procedure TNSToolbarHelper.AddFlexibleSpaceItem(AShowByDefault: Boolean);
begin
  DoAddItem('_SpaceItem' + IntToStr(FSpacerCount),
    TNSToolbarHelperItem.Create('NSToolbarFlexibleSpaceItem', AShowByDefault));
  Inc(FSpacerCount);
end;

procedure TNSToolbarHelper.AddSpaceItem(AShowByDefault: Boolean = True);
begin
  DoAddItem('_SpaceItem' + IntToStr(FSpacerCount),
    TNSToolbarHelperItem.Create('NSToolbarSpaceItem', AShowByDefault));
  Inc(FSpacerCount);
end;

procedure TNSToolbarHelper.Attach(AForm: TCommonCustomForm);
begin
  if AForm = nil then
  begin
    if FAttachedForm <> nil then NSWindowOfForm(FAttachedForm).setToolbar(nil);
    FAttachedForm := nil;
    Exit;
  end;
  NSWindowOfForm(AForm).setToolbar(FToolbar);
  FAttachedForm := AForm;
end;

function TNSToolbarHelper.CreateNSArrayOfItemIDs(ADefaultOnly: Boolean): NSArray;
var
  Counter: Integer;
  Item: TNSToolbarHelperItem;
  Strings: array of CFStringRef;
begin
  if ADefaultOnly then
    Counter := FDefaultItemCount
  else
    Counter := FItems.Count;
  SetLength(Strings, Counter);
  for Item in FItems do
    if not ADefaultOnly or Item.ShowByDefault then
    begin
      Strings[Length(Strings) - Counter] := CFStringCreateWithCharacters(nil,
        PChar(Item.ID), Length(Item.ID));
      Dec(Counter);
      if Counter = 0 then Break;
    end;
  Result := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@Strings[0], Length(Strings)));
  //the NSArray will now own the items, so release their implicit opening retain
  for Counter := 0 to High(Strings) do
    CFRelease(Strings[Counter]);
end;

function TNSToolbarHelper.GetItem(const ItemID: string): TNSToolbarHelperItem;
begin
  Result := FItemMap[ItemID];
end;

{ TNSToolbarHelper.TDelegate }

constructor TNSToolbarDelegate.Create(AOwner: TNSToolbarHelper);
begin
  inherited Create;
  FClickEventSelector := sel_getUid('ItemClicked:');
  FOwner := AOwner;
end;

destructor TNSToolbarDelegate.Destroy;
begin
  CFRelease(GetObjectID); //release the part of the object implemented on the Objective-C side
  inherited;
end;

function TNSToolbarDelegate.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(INSToolbarDelegate);
end;

procedure TNSToolbarDelegate.ItemClicked(Sender: NSToolbarItem);
var
  HelperItem: TNSToolbarHelperItem;
  Handler: TProc<TObject>;
begin
  HelperItem := FOwner[GetDelphiFromNSString(Sender.itemIdentifier)];
  Handler := HelperItem.OnClick;
  if Assigned(Handler) then
    try
      Handler(HelperItem);
    except
      Application.HandleException(ExceptObject);
    end;
end;

function TNSToolbarDelegate.toolbar(toolbar: Pointer;
  itemForItemIdentifier: CFStringRef; willBeInsertedIntoToolbar: Boolean): NSToolbarItem;
var
  ID: string;
begin
  ID := GetDelphiFromCFString(itemForItemIdentifier);
  Result := FOwner[ID].CocoaObject;
  //attach our 'action' handler
  Result.setTarget(GetObjectID);
  Result.setAction(FClickEventSelector);
end;

function TNSToolbarDelegate.toolbarAllowedItemIdentifiers(toolbar: Pointer): NSArray;
begin
  Result := FOwner.CreateNSArrayOfItemIDs(False);
end;

function TNSToolbarDelegate.toolbarDefaultItemIdentifiers(toolbar: Pointer): NSArray;
begin
  Result := FOwner.CreateNSArrayOfItemIDs(True);
end;

function TNSToolbarDelegate.validateToolbarItem(theItem: NSToolbarItem): Boolean;
var
  HelperItem: TNSToolbarHelperItem;
  Handler: TNSToolbarUpdateEvent;
begin
  HelperItem := FOwner[GetDelphiFromNSString(theItem.itemIdentifier)];
  Handler := HelperItem.OnUpdate;
  if Assigned(Handler) then
    try
      Handler(HelperItem, Result);
    except
      Application.HandleException(ExceptObject);
    end;
end;

end.

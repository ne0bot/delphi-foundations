unit CCR.MacPDFWriter;
{
  The OS X 2D graphics API ('Quartz') uses a PDF subset for its metafile format, and as
  a result, makes it easy to output PDFs using regular Quartz calls. This unit wraps the
  PDF creation functions to allow you to create a PDF by drawing to a FireMonkey TCanvas.

  To use, first choose whether you want to write directly to a file or to memory. If the
  former, use TPDFFileWriter, otherwise use TInMemoryPDFWriter. Either way, instantiate
  the chosen class, set properties for relevant metadata as required, then call
  BeginDoc, use the Canvas property to draw and the NewPage method to start a new page,
  and finally EndDoc to finish off. Once EndDoc has been called on a TInMemoryPDFWriter,
  call SaveToStream, SaveToFile and/or ToBytes to get the actual PDF data.

  Chris Rolliston, August 2012
  http://delphifoundations.com/
}
interface

uses
  System.Types, System.SysUtils, System.Classes, System.Rtti, FMX.Types, FMX.Forms,
  Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.CoreGraphics;

type
  EPDFWriterError = class(Exception);

  TPDFWriterBeginDocEvent = reference to procedure (AuxInfo: CFMutableDictionaryRef);

  TPDFWriter = class abstract
  strict private
    FAllowCopying, FAllowPrinting: Boolean;
    FAuthor, FCreator, FOwnerPassword, FSubject, FTitle, FUserPassword: string;
    FCanvas: TCanvas;
    FContextRef: CGContextRef;
    FKeywords: TStrings;
    FMediaBox: CGRect;
    FPageDictionary: CFDictionaryRef;
    FOnBeginDoc: TPDFWriterBeginDocEvent;
    procedure BoundsChanged;
    procedure DoNewPage;
    function GetBoundsRect: TRectF;
    procedure SetBoundsRect(const Value: TRectF);
    procedure SetLeft(const Value: Single);
    procedure SetTop(const Value: Single);
    procedure SetWidth(const Value: Single);
    procedure SetHeight(const Value: Single);
  protected
    procedure CheckStatus(ShouldBeWriting: Boolean);
    function CreateHandle(const AMediaBox: CGRect;
      const AAuxInfo: CFDictionaryRef): CGContextRef; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure BeginDoc;
    procedure EndDoc; virtual;
    procedure NewPage;
    //'auxiliary information' settings
    property AllowCopying: Boolean read FAllowCopying write FAllowCopying default True;
    property AllowPrinting: Boolean read FAllowPrinting write FAllowPrinting default True;
    property Author: string read FAuthor write FAuthor;
    property Creator: string read FCreator write FCreator;
    property Keywords: TStrings read FKeywords;
    property OwnerPassword: string read FOwnerPassword write FOwnerPassword;
    property Subject: string read FSubject write FSubject;
    property Title: string read FTitle write FTitle;
    property UserPassword: string read FUserPassword write FUserPassword;
    //'media box' settings
    property BoundsRect: TRectF read GetBoundsRect write SetBoundsRect;
    property Canvas: TCanvas read FCanvas;
    property ContextRef: CGContextRef read FContextRef;
    property Left: Single read FMediaBox.origin.x write SetLeft;
    property Top: Single read FMediaBox.origin.x write SetTop;
    property Width: Single read FMediaBox.size.width write SetWidth;
    property Height: Single read FMediaBox.size.height write SetHeight;
    property OnBeginDoc: TPDFWriterBeginDocEvent read FOnBeginDoc write FOnBeginDoc;
  end;

  TPDFFileWriter = class(TPDFWriter)
  strict private
    FFileName: string;
  protected
    function CreateHandle(const AMediaBox: CGRect;
      const AAuxInfo: CFDictionaryRef): CGContextRef; override;
  public
    constructor Create(const AFileName: string = ''); reintroduce;
    property FileName: string read FFileName write FFileName;
  end;

  TInMemoryPDFWriter = class(TPDFWriter)
  strict private
    FData: TMemoryStream;
    FDataConsumer: CGDataConsumerRef;
    class function PutBytesCallback(info: Pointer; buffer: Pointer; count: Longword): Longword; cdecl; static;
  protected
    function CreateHandle(const AMediaBox: CGRect;
      const AAuxInfo: CFDictionaryRef): CGContextRef; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    function Empty: Boolean; inline;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
    function ToBytes: TBytes;
  end;

implementation

resourcestring
  SBeginDocNotYetCalled = 'Cannot perform operation because BeginDoc is yet to be called';
  SCannotDoOpWhileActive = 'Cannot perform operation while document is being written';
  SWidthOrHeightZero = 'Neither Width nor Height may be 0';

var
  DefArrayCallbacks: CFArrayCallBacks;
  DefKeyCallbacks: CFDictionaryKeyCallBacks;
  DefValueCallbacks: CFDictionaryValueCallBacks;

procedure CoreReleaseAndNil(const Proc: Pointer; var Ref);
type
  TCoreReleaseProc = procedure (Ref: Pointer); cdecl;
var
  Temp: Pointer;
begin
  if Pointer(Ref) = nil then Exit; //CFRelease and friends crash on receipt of a nil reference
  Temp := Pointer(Ref);
  Pointer(Ref) := nil;
  TCoreReleaseProc(Proc)(Temp);
end;

procedure CFReleaseAndNil(var Ref); inline;
begin
  CoreReleaseAndNil(@CFRelease, Ref);
end;

procedure CGContextReleaseAndNil(var Ref: CGContextRef); inline;
begin
  CoreReleaseAndNil(@CGContextRelease, Ref);
end;

procedure CGDataConsumerReleaseAndNil(var Ref: CGDataConsumerRef); inline;
begin
  CoreReleaseAndNil(@CGDataConsumerRelease, Ref);
end;

function CFStringCreate(const S: string): CFStringRef; inline;
begin
  Result := CFStringCreateWithCharacters(nil, PChar(S), Length(S));
end;

function CFArrayCreateForStrings(Strings: TStrings): CFArrayRef;
var
  Arr: TArray<CFStringRef>;
  I: Integer;
begin
  SetLength(Arr, Strings.Count);
  for I := 0 to High(Arr) do
    Arr[I] := CFStringCreate(Strings[I]);
  Result := CFArrayCreate(nil, Arr, Length(Arr), @DefArrayCallbacks);
  for I := 0 to High(Arr) do
    CFReleaseAndNil(Arr[I]);
end;

{ TPDFWriter }

type
  TCanvasAccess = class(TCanvas);

{$IFDEF VER230} //backfill a few bits for XE2
  TCanvasManager = record
    class var RttiContext: TRttiContext;
    class var RttiField: TRttiField;
    class function DefaultCanvas: TCanvasClass; static; inline;
  end;

  TCanvasHelper = class helper for TCanvas
    function BeginScene(AClipRects: PClipRects = nil; AContextHandle: THandle = 0): Boolean;
  end;

class function TCanvasManager.DefaultCanvas: TCanvasClass;
begin
  Result := DefaultCanvasClass;
end;

function TCanvasHelper.BeginScene(AClipRects: PClipRects; AContextHandle: THandle): Boolean;
begin
  if TCanvasManager.RttiField = nil then
    TCanvasManager.RttiField := TCanvasManager.RttiContext.GetType(ClassType).GetField('FContext');
  TCanvasManager.RttiField.SetValue(Self, TValue.From(CGContextRef(AContextHandle)));
  Result := inherited BeginScene(AClipRects);
end;
{$ENDIF}

constructor TPDFWriter.Create;
begin
  inherited Create;
  FAllowCopying := True;
  FAllowPrinting := True;
  FCanvas := TCanvasManager.DefaultCanvas.Create;
  TCanvasAccess(FCanvas).Initialize;
  FKeywords := TStringList.Create;
end;

destructor TPDFWriter.Destroy;
begin
  CFReleaseAndNil(FPageDictionary);
  FKeywords.Free;
  FCanvas.Free;
  inherited;
end;

procedure TPDFWriter.BeforeDestruction;
begin
  if FContextRef <> nil then EndDoc;
  inherited;
end;

procedure TPDFWriter.BoundsChanged;
begin
  CFReleaseAndNil(FPageDictionary);
end;

procedure TPDFWriter.CheckStatus(ShouldBeWriting: Boolean);
begin
  if (FContextRef <> nil) <> ShouldBeWriting then
    if ShouldBeWriting then
      raise EPDFWriterError.CreateRes(@SBeginDocNotYetCalled)
    else
      raise EPDFWriterError.CreateRes(@SCannotDoOpWhileActive);
end;

procedure TPDFWriter.BeginDoc;
var
  AuxInfo: CFMutableDictionaryRef;

  procedure AddToAuxInfoAndRelease(const Key: string; CFValue: Pointer);
  begin
    CFDictionaryAddValue(AuxInfo, CFSTR(Key), CFValue);
    CFReleaseAndNil(CFValue);
  end;

  procedure AddToAuxInfo(const Key, Value: string); overload;
  begin
    if Value <> '' then
      AddToAuxInfoAndRelease(Key, CFStringCreate(Value));
  end;

  procedure AddToAuxInfo(const Key: string; Value: Boolean); overload;
  begin
    if Value then
      CFDictionarySetValue(AuxInfo, CFSTR(Key), kCFBooleanTrue)
    else
      CFDictionarySetValue(AuxInfo, CFSTR(Key), kCFBooleanFalse);
  end;
begin
  CheckStatus(False);
  if (Width = 0) or (Height = 0) then
    raise EPDFWriterError.CreateRes(@SWidthOrHeightZero);
  AuxInfo := CFDictionaryCreateMutable(nil, 0, @DefKeyCallbacks, @DefValueCallbacks);
  try
    //add the standard items
    AddToAuxInfo('kCGPDFContextAuthor', Author);
    AddToAuxInfo('kCGPDFContextCreator', Creator);
    AddToAuxInfo('kCGPDFContextSubject', Subject);
    AddToAuxInfo('kCGPDFContextTitle', Title);
    AddToAuxInfo('kCGPDFContextOwnerPassword', OwnerPassword);
    AddToAuxInfo('kCGPDFContextUserPassword', UserPassword);
    AddToAuxInfo('kCGPDFContextAllowsCopying', AllowCopying);
    AddToAuxInfo('kCGPDFContextAllowsPrinting', AllowPrinting);
    if Keywords.Count <> 0 then
      AddToAuxInfoAndRelease('kCGPDFContextKeywords', CFArrayCreateForStrings(Keywords));
    //give the calling code a chance to add to the dictionary
    if Assigned(FOnBeginDoc) then
      FOnBeginDoc(AuxInfo);
    //create the PDF graphics context
    FContextRef := CreateHandle(FMediaBox, CFDictionaryRef(AuxInfo));
    if FContextRef = nil then RaiseLastOSError;
  finally
    CFReleaseAndNil(AuxInfo);
  end;
  //initialize the canvas
  FCanvas.ResizeBuffer(Trunc(Width), Trunc(Height));
  DoNewPage;
end;

procedure TPDFWriter.EndDoc;
begin
  CheckStatus(True);
  FCanvas.EndScene;
  CGPDFContextEndPage(FContextRef);
  CGContextReleaseAndNil(FContextRef);
end;

procedure TPDFWriter.DoNewPage;
var
  KeyName: CFStringRef;
  MediaBoxData: CFDataRef;
begin
  if FPageDictionary = nil then
  begin
    KeyName := CFSTR('kCGPDFContextMediaBox');
    MediaBoxData := CFDataCreate(nil, @FMediaBox, SizeOf(FMediaBox));
    FPageDictionary := CFDictionaryCreate(nil, @KeyName, @MediaBoxData, 1,
      @DefKeyCallbacks, @DefValueCallbacks);
    CFReleaseAndNil(MediaBoxData);
  end;
  CGPDFContextBeginPage(FContextRef, FPageDictionary);
  FCanvas.BeginScene(nil, THandle(FContextRef));
end;

procedure TPDFWriter.NewPage;
begin
  CheckStatus(True);
  FCanvas.EndScene;
  CGPDFContextEndPage(FContextRef);
  DoNewPage;
end;

function TPDFWriter.GetBoundsRect: TRectF;
begin
  Result.Left := FMediaBox.origin.x;
  Result.Top := FMediaBox.origin.y;
  Result.Width := FMediaBox.size.width;
  Result.Height := FMediaBox.size.height;
end;

procedure TPDFWriter.SetBoundsRect(const Value: TRectF);
begin
  FMediaBox.origin.x := Value.Left;
  FMediaBox.origin.y := Value.Top;
  FMediaBox.size.width := Value.Width;
  FMediaBox.size.height := Value.Height;
  BoundsChanged;
end;

procedure TPDFWriter.SetHeight(const Value: Single);
begin
  if Value = Height then Exit;
  FMediaBox.size.height := Value;
  BoundsChanged;
end;

procedure TPDFWriter.SetLeft(const Value: Single);
begin
  if Value = Left then Exit;
  FMediaBox.origin.x := Value;
  BoundsChanged;
end;

procedure TPDFWriter.SetTop(const Value: Single);
begin
  if Value = Top then Exit;
  FMediaBox.origin.y := Value;
  BoundsChanged;
end;

procedure TPDFWriter.SetWidth(const Value: Single);
begin
  if Value = Width then Exit;
  FMediaBox.size.width := Value;
  BoundsChanged;
end;

{ TPDFFileWriter }

constructor TPDFFileWriter.Create(const AFileName: string);
begin
  inherited Create;
  FileName := AFileName;
end;

function TPDFFileWriter.CreateHandle(const AMediaBox: CGRect;
  const AAuxInfo: CFDictionaryRef): CGContextRef;
var
  Path: CFStringRef;
  URL: CFURLRef;
begin
  Path := CFStringCreateWithCharacters(nil, PChar(FileName), Length(FileName));
  URL := CFURLCreateWithFileSystemPath(nil, Path, kCFURLPOSIXPathStyle, False);
  Result := CGPDFContextCreateWithURL(URL, @AMediaBox, AAuxInfo);
  CFReleaseAndNil(URL);
  CFReleaseAndNil(Path);
end;

{ TInMemoryPDFWriter }

constructor TInMemoryPDFWriter.Create;
var
  Callbacks: CGDataConsumerCallbacks;
begin
  inherited;
  FData := TMemoryStream.Create;
  Callbacks.putBytes := TInMemoryPDFWriter.PutBytesCallback;
  Callbacks.releaseConsumer := nil;
  FDataConsumer := CGDataConsumerCreate(FData, @Callbacks)
end;

destructor TInMemoryPDFWriter.Destroy;
begin
  CGDataConsumerReleaseAndNil(FDataConsumer);
  FData.Free;
  inherited;
end;

class function TInMemoryPDFWriter.PutBytesCallback(info, buffer: Pointer;
  count: Longword): Longword;
begin
  Result := TStream(info).Write(buffer^, count);
end;

function TInMemoryPDFWriter.CreateHandle(const AMediaBox: CGRect;
  const AAuxInfo: CFDictionaryRef): CGContextRef;
begin
  Clear;
  Result := CGPDFContextCreate(FDataConsumer, @AMediaBox, AAuxInfo);
end;

procedure TInMemoryPDFWriter.Clear;
begin
  FData.Clear;
end;

function TInMemoryPDFWriter.Empty: Boolean;
begin
  Result := FData.Size > 0;
end;

procedure TInMemoryPDFWriter.SaveToFile(const AFileName: string);
begin
  CheckStatus(False);
  FData.SaveToFile(AFileName);
end;

procedure TInMemoryPDFWriter.SaveToStream(AStream: TStream);
begin
  CheckStatus(False);
  FData.SaveToStream(AStream);
end;

function TInMemoryPDFWriter.ToBytes: TBytes;
begin
  CheckStatus(False);
  SetLength(Result, FData.Size);
  if Result <> nil then Move(FData.Memory^, Result[0], Length(Result));
end;

initialization
  DefArrayCallbacks := kCFTypeArrayCallBacks;
  DefKeyCallbacks := kCFTypeDictionaryKeyCallBacks;
  DefValueCallbacks := kCFTypeDictionaryValueCallBacks;
end.

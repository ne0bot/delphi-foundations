unit ScreenshotForm;
{
  Example of taking a screenshot on Windows and OS X. The the latter case the
  code is a bit tedious because the OS X's straight C APIs are a bit tedious, if
  smoothly written.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Objects;

type
  TfrmScreenshot = class(TForm)
    panFrame: TPanel;
    lyoHeader: TLayout;
    btnTakeScreenshot: TButton;
    imgDest: TImage;
    procedure btnTakeScreenshotClick(Sender: TObject);
  end;

var
  frmScreenshot: TfrmScreenshot;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows, Vcl.Graphics, FMX.Platform;
  {$ENDIF}
  {$IFDEF MACOS}
  Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.CoreGraphics, Macapi.ImageIO;
  {$ENDIF}

{$R *.fmx}

{$IFDEF MSWINDOWS}
type
  TBitmap = FMX.Types.TBitmap;
  TVclBitmap = Vcl.Graphics.TBitmap;

procedure TakeScreenshot(Dest: TBitmap);
var
  DC: HDC;
  Size: TPointF;
  VCLBitmap: TVclBitmap;
  Y: Integer;
begin
  VCLBitmap := nil;
  Size := Platform.GetScreenSize;
  DC := GetDC(0);
  try
    VCLBitmap := TVclBitmap.Create;
    VCLBitmap.PixelFormat := pf32bit;
    VCLBitmap.SetSize(Trunc(Size.X), Trunc(Size.Y));
    BitBlt(VCLBitmap.Canvas.Handle, 0, 0, VCLBitmap.Width, VCLBitmap.Height,
      DC, 0, 0, SRCCOPY);
    Dest.SetSize(VCLBitmap.Width, VCLBitmap.Height);
    { The format of a FMX bitmap and a 32 bit VCL bitmap is the same, so just
      copy the scanlines. }
    for Y := Dest.Height - 1 downto 0 do
      Move(VCLBitmap.ScanLine[Y]^, Dest.ScanLine[Y]^, Dest.Width * 4);
  finally
    ReleaseDC(0, DC);
    VCLBitmap.Free;
  end;
end;
{$ENDIF}

{$IFDEF MACOS}

{$IF NOT DECLARED(CGRectInfinite)}
const
  CGRectInfinite: CGRect = (origin: (x: -8.98847e+30; y: -8.98847e+307);
    size: (width: 1.79769e+308; height: 1.79769e+308));
{$IFEND}

function PutBytesCallback(Stream: TStream; NewBytes: Pointer; Count: LongInt): LongInt; cdecl;
begin
  Result := Stream.Write(NewBytes^, Count);
end;

procedure ReleaseConsumerCallback(Dummy: Pointer); cdecl;
begin
end;

procedure WriteCGImageToStream(const AImage: CGImageRef; AStream: TStream;
  const AType: string = 'public.png'; AOptions: CFDictionaryRef = nil);
var
  Callbacks: CGDataConsumerCallbacks;
  Consumer: CGDataConsumerRef;
  ImageDest: CGImageDestinationRef;
  TypeCF: CFStringRef;
begin
  Callbacks.putBytes := @PutBytesCallback;
  Callbacks.releaseConsumer := ReleaseConsumerCallback;
  ImageDest := nil;
  TypeCF := nil;
  Consumer := CGDataConsumerCreate(AStream, @Callbacks);
  if Consumer = nil then RaiseLastOSError;
  try
    TypeCF := CFStringCreateWithCharactersNoCopy(nil, PChar(AType), Length(AType), kCFAllocatorNull);
    ImageDest := CGImageDestinationCreateWithDataConsumer(Consumer, TypeCF, 1, AOptions);
    if ImageDest = nil then RaiseLastOSError;
    CGImageDestinationAddImage(ImageDest, AImage, nil);
    if CGImageDestinationFinalize(ImageDest) = 0 then RaiseLastOSError;
  finally
    if ImageDest <> nil then CFRelease(ImageDest);
    if TypeCF <> nil then CFRelease(TypeCF);
    CGDataConsumerRelease(Consumer);
  end;
end;

procedure TakeScreenshot(Dest: TBitmap);
var
  Screenshot: CGImageRef;
  Stream: TMemoryStream;
begin
  Stream := nil;
  ScreenShot := CGWindowListCreateImage(CGRectInfinite,
    kCGWindowListOptionOnScreenOnly, kCGNullWindowID, kCGWindowImageDefault);
  if ScreenShot = nil then RaiseLastOSError;
  try
    Stream := TMemoryStream.Create;
    WriteCGImageToStream(ScreenShot, Stream);
    Stream.Position := 0;
    Dest.LoadFromStream(Stream);
  finally
    CGImageRelease(ScreenShot);
    Stream.Free;
  end;
end;
{$ENDIF}

procedure TfrmScreenshot.btnTakeScreenshotClick(Sender: TObject);
begin
  TakeScreenshot(imgDest.Bitmap);
end;

end.

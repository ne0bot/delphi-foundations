unit PDFViewerForm;
{
  Simple OS X PDF viewer that uses the Core Graphics API - inspired by the
  example in Apple's Core Graphics documentation (https://developer.apple.com/
  library/mac/#documentation/graphicsimaging/conceptual/drawingwithquartz2d/
  dq_pdf/dq_pdf.html#//apple_ref/doc/uid/TP30001066-CH214-TPXREF101).
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Effects, FMX.Objects,
  FMX.Menus, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.CoreGraphics;

type
  TfrmPDFViewer = class(TForm)
    btnPrevPage: TSpeedButton;
    Line1: TLine;
    BevelEffect1: TBevelEffect;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    itmFileOpen: TMenuItem;
    dlgOpen: TOpenDialog;
    pbxPDF: TPaintBox;
    Image1: TImage;
    Label1: TLabel;
    btnNextPage: TSpeedButton;
    Image2: TImage;
    Label2: TLabel;
    btnRotateLeft: TSpeedButton;
    Image3: TImage;
    Label3: TLabel;
    btnRotateRight: TSpeedButton;
    Image4: TImage;
    Label4: TLabel;
    panButtons: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure itmFileOpenClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbxPDFPaint(Sender: TObject; Canvas: TCanvas);
    procedure btnPrevPageClick(Sender: TObject);
    procedure btnNextPageClick(Sender: TObject);
    procedure btnRotateLeftClick(Sender: TObject);
    procedure btnRotateRightClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    FDocRef: CGPDFDocumentRef;
    FMainCaption: string;
    FPageRef: CGPDFPageRef;
    FPageCount, FPageIndex, FRotation: Integer;
    procedure FreeDocRef;
    procedure SetPageIndex(NewIndex: Integer);
  end;

var
  frmPDFViewer: TfrmPDFViewer;

implementation

uses System.Rtti;

{$R *.fmx}

{ It's a crappy hack, but FMX at present doesn't have the native API hooks the
  VCL does. }
function GetCGContextFromCanvas(ACanvas: TCanvas): CGContextRef;
var
  Context: TRttiContext;
  Field: TRttiField;
begin
  Field := Context.GetType(ACanvas.ClassType).GetField('FContext');
  Assert(Field <> nil);
  Result := PPointer(Field.GetValue(ACanvas).GetReferenceToRawData)^;
end;

{ Helper function to load a CGPDFDocument from an existing PDF file. }
function OpenCGPDFDocument(const AFileName: string): CGPDFDocumentRef;
var
  Path: CFStringRef;
  URL: CFURLRef;
begin
  Path := CFStringCreateWithCharacters(nil, PChar(AFileName), Length(AFileName));
  URL := CFURLCreateWithFileSystemPath(nil, Path, kCFURLPOSIXPathStyle, False);
  Result := CGPDFDocumentCreateWithURL(URL);
  CFRelease(URL);
  CFRelease(Path);
end;

procedure TfrmPDFViewer.FormCreate(Sender: TObject);
begin
  FMainCaption := Caption;
  itmFileOpen.ShortCut := scCommand or Ord('O');
end;

procedure TfrmPDFViewer.FormDestroy(Sender: TObject);
begin
  FreeDocRef;
end;

procedure TfrmPDFViewer.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  case Key of
    vkLeft: if btnPrevPage.Enabled then btnPrevPageClick(nil) else Beep;
    vkRight: if btnNextPage.Enabled then btnNextPageClick(nil) else Beep;
    vkHome: SetPageIndex(1);
    vkEnd: SetPageIndex(FPageCount);
  end;
end;

procedure TfrmPDFViewer.FreeDocRef;
begin
  if FDocRef <> nil then Exit;
  CGPDFDocumentRelease(FDocRef);
  FDocRef := nil;
end;

procedure TfrmPDFViewer.SetPageIndex(NewIndex: Integer);
begin
  if FDocRef = nil then
    Caption := FMainCaption
  else
  begin
    Caption := Format('%s (%d of %d)', [FMainCaption, NewIndex, FPageCount]);
    FPageRef := CGPDFDocumentGetPage(FDocRef, NewIndex);
  end;
  FPageIndex := NewIndex;
  btnPrevPage.Enabled := (NewIndex > 1);
  btnNextPage.Enabled := (NewIndex < FPageCount);
  pbxPDF.Repaint;
end;

procedure TfrmPDFViewer.itmFileOpenClick(Sender: TObject);
begin
  if not dlgOpen.Execute then Exit;
  FreeDocRef;
  FDocRef := OpenCGPDFDocument(dlgOpen.FileName);
  if FDocRef <> nil then
    FPageCount := CGPDFDocumentGetNumberOfPages(FDocRef)
  else
    FPageCount := 0;
  FRotation := 0;
  btnRotateLeft.Enabled := (FPageCount > 0);
  btnRotateRight.Enabled := (FPageCount > 0);
  SetPageIndex(1);
end;

procedure TfrmPDFViewer.pbxPDFPaint(Sender: TObject; Canvas: TCanvas);
var
  Context: CGContextRef;
  R: NSRect;
begin
  if FPageRef = nil then Exit;
  Context := GetCGContextFromCanvas(Canvas);
  if Context = nil then Exit;
  R.origin.x := 0; R.origin.y := 0;
  R.size.width := pbxPDF.Width;
  R.size.height := pbxPDF.Height;
  CGContextSaveGState(Context);
  try
    CGContextTranslateCTM(Context, 0, R.size.height);
    CGContextScaleCTM(Context, 1, -1);
    CGContextConcatCTM(Context, CGPDFPageGetDrawingTransform(FPageRef,
      kCGPDFCropBox, R, FRotation, Ord(True)));
    CGContextDrawPDFPage(Context, FPageRef);
  finally
    CGContextRestoreGState(Context);
  end;
end;

procedure TfrmPDFViewer.btnPrevPageClick(Sender: TObject);
begin
  SetPageIndex(FPageIndex - 1);
end;

procedure TfrmPDFViewer.btnNextPageClick(Sender: TObject);
begin
  SetPageIndex(FPageIndex + 1);
end;

procedure TfrmPDFViewer.btnRotateLeftClick(Sender: TObject);
begin
  Dec(FRotation, 90);
  if FRotation = -360 then FRotation := 0;
  pbxPDF.Repaint;
end;

procedure TfrmPDFViewer.btnRotateRightClick(Sender: TObject);
begin
  Inc(FRotation, 90);
  if FRotation = 360 then FRotation := 0;
  pbxPDF.Repaint;
end;

end.


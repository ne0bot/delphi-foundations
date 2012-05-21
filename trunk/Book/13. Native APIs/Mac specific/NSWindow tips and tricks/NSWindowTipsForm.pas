unit NSWindowTipsForm;
{
  While a FireMonkey control is not a native control, a FireMonkey form is a native form, which on
  OS X means a Cocoa NSWindow. This program demos some NSWindow features not exposed by the
  FireMonkey form class directly.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects;

type
  TfrmNSWindowTips = class(TForm)
    MinSizeSelector: TSelection;
    btnToggleZoom: TButton;
    chkConstrainAspectRatio: TCheckBox;
    chkDragFromAnywhere: TCheckBox;
    btnOpenFile: TButton;
    btnCreatePDF: TButton;
    btnConstrainMaxSize: TButton;
    btnCenter: TButton;
    btnUnconstrainMaxSize: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    btnConstrainMinSize: TButton;
    btnUnconstrainMinSize: TButton;
    Label1: TLabel;
    procedure btnCenterClick(Sender: TObject);
    procedure btnToggleZoomClick(Sender: TObject);
    procedure btnConstrainMaxSizeClick(Sender: TObject);
    procedure btnUnconstrainMaxSizeClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCreatePDFClick(Sender: TObject);
    procedure chkDragFromAnywhereChange(Sender: TObject);
    procedure chkConstrainAspectRatioChange(Sender: TObject);
    procedure btnConstrainMinSizeClick(Sender: TObject);
    procedure btnUnconstrainMinSizeClick(Sender: TObject);
  end;

var
  frmNSWindowTips: TfrmNSWindowTips;

implementation

{$R *.fmx}

uses
  System.Math, Macapi.ObjectiveC, Macapi.CocoaTypes, Macapi.Foundation, Macapi.AppKit,
  FMX.Platform, FMX.Platform.Mac;

{ helper routines }

type
  TOCLocalAccess = class(TOCLocal);

function NSWindowOfForm(Form: TCommonCustomForm): NSWindow;
var
  Obj: TOCLocal;
begin
  Obj := (FmxHandleToObjC(Form.Handle) as TOCLocal);
  Result := NSWindow(TOCLocalAccess(Obj).Super);
end;

function GetSharedNSWorkspace: NSWorkspace; inline;
begin
  Result := TNSWorkspace.Wrap(TNSWorkspace.OCClass.sharedWorkspace);
end;

function GetNSClientSizeOfForm(Form: TCommonCustomForm): NSSize;
var
  AsPoint: TPointF;
begin
  AsPoint := Platform.GetClientSize(Form);
  Result.width := AsPoint.X;
  Result.height := AsPoint.Y;
end;

procedure CreatePDFFromForm(AForm: TCommonCustomForm; ADestStream: TStream); overload;
var
  Data: NSData;
  Rect: NSRect;
begin
  Rect.origin.x := 0;
  Rect.origin.y := 0;
  Rect.size := GetNSClientSizeOfForm(AForm);
  Data := NSWindowOfForm(AForm).dataWithPDFInsideRect(Rect);
  if (Data = nil) or (Data.length = 0) then RaiseLastOSError;
  ADestStream.WriteBuffer(Data.bytes^, Data.length);
end;

function MakeNSSize(AWidth, AHeight: Single): NSSize;
begin
  Result.width := AWidth;
  Result.height := AHeight;
end;

{ TfrmCocoaTips }

procedure TfrmNSWindowTips.FormCreate(Sender: TObject);
begin
  //auto-save size and position
  NSWindowOfForm(Self).setFrameAutosaveName(NSSTR(Name))
end;

procedure TfrmNSWindowTips.btnCenterClick(Sender: TObject);
begin
  NSWindowOfForm(Self).center;
end;

procedure TfrmNSWindowTips.btnConstrainMaxSizeClick(Sender: TObject);
begin
  NSWindowOfForm(Self).setContentMaxSize(GetNSClientSizeOfForm(Self));
end;

procedure TfrmNSWindowTips.btnConstrainMinSizeClick(Sender: TObject);
var
  R: TRectF;
  MinClientSize: NSSize;
begin
  R := MinSizeSelector.BoundsRect;
  MinClientSize.width := R.Right;
  MinClientSize.height := R.Bottom;
  NSWindowOfForm(Self).setContentMinSize(MinClientSize);
end;

procedure TfrmNSWindowTips.btnOpenFileClick(Sender: TObject);
begin
  if not dlgOpen.Execute then Exit;
  NSWindowOfForm(Self).setRepresentedFilename(NSSTR(dlgOpen.FileName));
end;

procedure TfrmNSWindowTips.btnToggleZoomClick(Sender: TObject);
begin
  NSWindowOfForm(Self).performZoom(nil);
end;

procedure TfrmNSWindowTips.btnUnconstrainMaxSizeClick(Sender: TObject);
begin
  NSWindowOfForm(Self).setContentMaxSize(MakeNSSize(MaxSingle, MaxSingle));
end;

procedure TfrmNSWindowTips.btnUnconstrainMinSizeClick(Sender: TObject);
begin
  NSWindowOfForm(Self).setContentMinSize(MakeNSSize(0, 0));
end;

procedure TfrmNSWindowTips.chkConstrainAspectRatioChange(Sender: TObject);
var
  Ratio: NSSize;
begin
  if chkConstrainAspectRatio.IsChecked then
    Ratio := GetNSClientSizeOfForm(Self) //just constrain to the current aspect ratio
  else
  begin
    Ratio.width := 0;
    Ratio.height := 0;
  end;
  NSWindowOfForm(Self).setContentAspectRatio(Ratio);
end;

procedure TfrmNSWindowTips.chkDragFromAnywhereChange(Sender: TObject);
begin
  NSWindowOfForm(Self).setMovableByWindowBackground(chkDragFromAnywhere.IsChecked);
end;

procedure TfrmNSWindowTips.btnCreatePDFClick(Sender: TObject);
var
  Stream: TFileStream;
begin
  if not dlgSave.Execute then Exit;
  Stream := TFileStream.Create(dlgSave.FileName, fmCreate);
  try
    CreatePDFFromForm(Self, Stream);
  finally
    Stream.Free;
  end;
  //open the newly-created PDF in the default PDF application (typically Preview)
  GetSharedNSWorkspace.openFile(NSSTR(dlgSave.FileName));
end;

end.

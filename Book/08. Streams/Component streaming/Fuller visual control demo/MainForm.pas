unit MainForm;
{
  Simple demo of the ReadComponent and WriteComponent methods of TStream: click
  on the buttons listed under 'Objects' to add things to the scroll box, before
  resizing and repositioning them as you wish (a few settings are also available
  by right-clicking on an object). Then, click 'Save to File' to persist the
  current set of objects to disk and 'Load from File' to restore them. Also
  demonstrates supporting both space-saving binary and human-readable text
  formats (binary is the default).

  Things to note:
  - We persist the scroll box that the dynamically-created controls are parented
    to. The crucial thing is that the scroll box is those controls' *owner*
    rather than their parent however. Establishing this is done by passing a
    reference to the scroll box to the child object's constructor.
  - ReadComponent creates child objects afresh, so we need to free any existing
    ones first.
  - In FireMonkey, all stock control types are automatically registered with the
    component streaming system. Custom controls - like our TContainer - must be
    explicitly registered using RegisterFMXClasses. In the VCL there are
    a couple of differences - firstly, the registration routine to call is
    RegisterClass or RegisterClasses, and secondly, you typically have to call
    it for standard control classes too. Calling either RegisterFMXClasses or
    RegisterClasses is usually done in a unit's initialization section.

  While the way you would go about implementing a control that the user could
  visually change the size and position of in a VCL application would be a bit
  different to the FireMonkey code presented here, the actual streaming code
  would be identical.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.Menus, Containers;

type
  ///	<summary>
  ///	  Interposer class to backfill the OnPopup event, which is missing in the
  ///	  FMX TPopupMenu.
  ///	</summary>
  TPopupMenu = class(FMX.Menus.TPopupMenu)
  strict private
    FOnPopup: TNotifyEvent;
  public
    procedure Popup(X, Y: Single); override;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
  end;

  TfrmStreamingDemo = class(TForm)
    scbHost: TFramedScrollBox;
    lyoButtons: TLayout;
    btnSaveToFile: TButton;
    btnLoadFromFile: TButton;
    dlgSave: TSaveDialog;
    btnClear: TButton;
    dlgOpen: TOpenDialog;
    btnAddScisscors: TButton;
    imgScissors: TImage;
    btnClock: TButton;
    imgClock: TImage;
    btnHouse: TButton;
    imgHouse: TImage;
    mnuControl: TPopupMenu;
    itmBringToFront: TMenuItem;
    itmSendToBack: TMenuItem;
    btnAddCustom: TButton;
    Line1: TLine;
    Line2: TLine;
    dlgAddCustom: TOpenDialog;
    MenuItem1: TMenuItem;
    itmRemove: TMenuItem;
    lblPictures: TLabel;
    lblPictureGroups: TLabel;
    itmResetSize: TMenuItem;
    MenuItem2: TMenuItem;
    itmLockAspectRatio: TMenuItem;
    itmAllowMovingAndSizing: TMenuItem;
    chkSaveAsText: TCheckBox;
    btnAddFromFile: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnSaveToFileClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnLoadFromFileClick(Sender: TObject);
    procedure btnAddControlClick(Sender: TObject);
    procedure mnuControlPopup(Sender: TObject);
    procedure itmBringToFrontClick(Sender: TObject);
    procedure itmSendToBackClick(Sender: TObject);
    procedure btnAddCustomClick(Sender: TObject);
    procedure itmRemoveClick(Sender: TObject);
    procedure itmResetSizeClick(Sender: TObject);
    procedure itmLockAspectRatioClick(Sender: TObject);
    procedure itmAllowMovingAndSizingClick(Sender: TObject);
    procedure btnAddFromFileClick(Sender: TObject);
    procedure scbHostMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  strict private
    procedure AddControl(ABitmap: TBitmap);
    procedure ClearAddedControls;
    function GetClickedContainer: TContainer; inline;
    function GetContainedImage(AContainer: TContainer): TImage; inline;
    procedure ShowLoadFromFileDialog(ClearFirst: Boolean);
  end;

var
  frmStreamingDemo: TfrmStreamingDemo;

implementation

{$R *.fmx}

{ TPopupMenu }

procedure TPopupMenu.Popup(X, Y: Single);
begin
  if Assigned(FOnPopup) then FOnPopup(Self);
  inherited;
end;

{ utility methods }

procedure TfrmStreamingDemo.AddControl(ABitmap: TBitmap);
var
  NewImage: TImage;
  NewContainer: TContainer;
begin
  NewContainer := TContainer.Create(scbHost);
  NewImage := TImage.Create(NewContainer);
  NewImage.Align := TAlignLayout.alClient;
  NewImage.Bitmap.Assign(ABitmap);
  NewImage.HitTest := False;
  NewImage.Parent := NewContainer;
  NewImage.WrapMode := TImageWrapMode.iwStretch;
  NewContainer.Position.X := 20 * scbHost.ComponentCount;
  NewContainer.Position.Y := 20 * scbHost.ComponentCount;
  NewContainer.SetClientSize(ABitmap.Width, ABitmap.Height);
  NewContainer.Parent := scbHost;
  NewContainer.PopupMenu := mnuControl;
  NewContainer.SetFocus;
end;

procedure TfrmStreamingDemo.ClearAddedControls;
var
  I: Integer;
begin
  for I := scbHost.ComponentCount - 1 downto 0 do
    if scbHost.Components[I] is TContainer then
      scbHost.Components[I].Free;
end;

function TfrmStreamingDemo.GetClickedContainer: TContainer;
begin
  Result := mnuControl.PopupComponent as TContainer;
end;

function TfrmStreamingDemo.GetContainedImage(AContainer: TContainer): TImage;
begin
  Result := AContainer.Children[0] as TImage;
end;

procedure TfrmStreamingDemo.ShowLoadFromFileDialog(ClearFirst: Boolean);
var
  FileStream: TFileStream;
  MemStream: TMemoryStream;
begin
  if not dlgOpen.Execute then Exit;
  if ClearFirst then ClearAddedControls;
  MemStream := nil;
  FileStream := TFileStream.Create(dlgOpen.FileName, fmOpenRead);
  try
    if TestStreamFormat(FileStream) = sofBinary then
      FileStream.ReadComponent(scbHost)
    else
    begin
      MemStream := TMemoryStream.Create;
      ObjectTextToBinary(FileStream, MemStream);
      MemStream.Seek(0, soBeginning);
      MemStream.ReadComponent(scbHost)
    end;
  finally
    FileStream.Free;
    MemStream.Free;
  end;
  Realign; //calling this ensures the scroll bars will be created correctly (small FMX bug)
end;

{ event handlers }

procedure TfrmStreamingDemo.FormCreate(Sender: TObject);
begin
  dlgAddCustom.Filter := Format(dlgAddCustom.Filter, [DefaultBitmapCodecClass.GetFileTypes]);
  mnuControl.OnPopup := mnuControlPopup;
end;

procedure TfrmStreamingDemo.btnAddControlClick(Sender: TObject);
begin
  AddControl(((Sender as TControl).Children[1] as TImage).Bitmap);
end;

procedure TfrmStreamingDemo.btnAddCustomClick(Sender: TObject);
var
  Bitmap: TBitmap;
  FileName: string;
begin
  if not dlgAddCustom.Execute then Exit;
  Bitmap := TBitmap.Create(0, 0);
  try
    for FileName in dlgAddCustom.Files do
    begin
      Bitmap.LoadFromFile(FileName);
      AddControl(Bitmap);
    end;
  finally
    Bitmap.Free;
  end;
end;

procedure TfrmStreamingDemo.btnAddFromFileClick(Sender: TObject);
begin
  ShowLoadFromFileDialog(False);
end;

procedure TfrmStreamingDemo.btnClearClick(Sender: TObject);
begin
  ClearAddedControls;
end;

procedure TfrmStreamingDemo.btnLoadFromFileClick(Sender: TObject);
begin
  ShowLoadFromFileDialog(True);
end;

procedure TfrmStreamingDemo.btnSaveToFileClick(Sender: TObject);
var
  FileStream: TFileStream;
  MemStream: TMemoryStream;
begin
  if not dlgSave.Execute then Exit;
  MemStream := nil;
  FileStream := TFileStream.Create(dlgSave.FileName, fmCreate);
  try
    if not chkSaveAsText.IsChecked then
      FileStream.WriteComponent(scbHost)
    else
    begin
      MemStream := TMemoryStream.Create;
      MemStream.WriteComponent(scbHost);
      MemStream.Seek(0, soBeginning);
      ObjectBinaryToText(MemStream, FileStream);
    end;
  finally
    FileStream.Free;
    MemStream.Free;
  end;
end;

procedure TfrmStreamingDemo.scbHostMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  Focused := nil;
end;

procedure TfrmStreamingDemo.mnuControlPopup(Sender: TObject);
var
  Item: TMenuItem;
  Container: TContainer;
begin
  Container := GetClickedContainer;
  itmAllowMovingAndSizing.IsChecked := Container.AllowMovingAndResizing;
  itmLockAspectRatio.IsChecked := Container.Proportional;
  for Item in TArray<TMenuItem>.Create(itmBringToFront, itmSendToBack, itmLockAspectRatio, itmResetSize) do
    Item.Enabled := Container.AllowMovingAndResizing;
end;

procedure TfrmStreamingDemo.itmBringToFrontClick(Sender: TObject);
begin
  GetClickedContainer.BringToFront;
end;

procedure TfrmStreamingDemo.itmSendToBackClick(Sender: TObject);
begin
  GetClickedContainer.SendToBack;
end;

procedure TfrmStreamingDemo.itmLockAspectRatioClick(Sender: TObject);
begin
  GetClickedContainer.Proportional := itmLockAspectRatio.IsChecked;
end;

procedure TfrmStreamingDemo.itmResetSizeClick(Sender: TObject);
var
  Container: TContainer;
  Image: TImage;
begin
  Container := GetClickedContainer;
  Image := GetContainedImage(Container);
  Container.SetClientSize(Image.Bitmap.Width, Image.Bitmap.Height);
end;

procedure TfrmStreamingDemo.itmAllowMovingAndSizingClick(Sender: TObject);
begin
  GetClickedContainer.AllowMovingAndResizing := itmAllowMovingAndSizing.IsChecked;
end;

procedure TfrmStreamingDemo.itmRemoveClick(Sender: TObject);
begin
  GetClickedContainer.Free;
end;

end.

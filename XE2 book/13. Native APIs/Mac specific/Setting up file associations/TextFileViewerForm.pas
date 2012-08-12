{
  Demonstrates how to enable opening files on OS X via either (i) the menu bar
  (File|Open...), (ii) Cmd+O, (iii) dragging and dropping files from Finder onto an
  open instance of the application's main form, (iv) dragging and dropping files onto
  the application's Dock icon, and (v) double clicking on a file with an associated
  file type (in the example's case, text files with a .tfvdoc extension). In
  implementing this, (i) is just a matter of adding the menu item, (ii) assigning the
  menu item's Shortcut property at runtime, and (iii) using FMX's built-in support for it.
  The fourth and fifth things go together, and involve two steps:
  1. Providing a custom delegate object for NSApplication in order to handle the
     application:openFile: message. In the demo, this is implemented in
     CustomNSApplicationDelegate.pas.
  2. Adding the necessary registration keys in the application's Info.plist file.
  Unfortunately, while you can add additional keys to the default Info.plist file in
  the IDE (see the 'Version Info' node of Project Options when targeting OS X), you
  cannot add nested additional keys specifically, which is what we need here.

  Nonetheless, it is easy to deploy a custom Info.plist file - do so by heading into the
  Deployment tab via Project|Deployment, selecting 'All configurations - OS X platform',
  then adding the custom Info.plist (set its Remote Path to Contents\Resources) before
  uncehcking the two default Info.plist entries. In the Info.plist file for this demo,
  I've commented the XML, so check it out for what keys to set.

  Note: since OS X caches plist data, when you change an application's plist file, you
  may have a hard time getting the system to pick up the new settings. Sometimes
  renaming the bundle to something else before renaming it back again works, other
  times not...

  Note 2: this demo is complementary to others written for the book Delphi XE2
  Foundations by me, Chris Rolliston. The book is available now on Amazon in both
  printed and Kindle eBook versions - see http://delphifoundations.com for more details.
}
unit TextFileViewerForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Memo, FMX.TabControl, FMX.Menus,
  FMX.Layouts, FMX.ListBox, FMX.Ani, FMX.Objects;

type
  TLoadedFile = class
    FileName, Contents: string;
    ListItem: TListBoxItem;
  end;

  TfrmMain = class(TForm)
    lblFileName: TLabel;
    MainMenu1: TMainMenu;
    itmFile: TMenuItem;
    itmExport: TMenuItem;
    dlgSave: TSaveDialog;
    itmOpen: TMenuItem;
    dlgOpen: TOpenDialog;
    lsbFiles: TListBox;
    Splitter1: TSplitter;
    memDisplay: TMemo;
    CloseButtonTemplate: TEllipse;
    txtClose: TText;
    FloatAnimation1: TFloatAnimation;
    procedure FormCreate(Sender: TObject);
    procedure itmExportClick(Sender: TObject);
    procedure itmOpenClick(Sender: TObject);
    procedure lsbFilesChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FLoadedFiles: TObjectDictionary<string, TLoadedFile>;
    FHandleDragDirectly: Boolean;
    procedure CloseButtonClick(Sender: TObject);
    procedure OpenFile(const AFileName: string);
  public
    procedure DragEnter(const Data: TDragObject; const Point: TPointF); override;
    procedure DragOver(const Data: TDragObject; const Point: TPointF; var Accept: Boolean); override;
    procedure DragDrop(const Data: TDragObject; const Point: TPointF); override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  System.IOUtils, System.Math, Macapi.CocoaTypes, Macapi.Foundation, Macapi.AppKit,
  CustomNSApplicationDelegate;

{ This procedure shows a native message box instead of FMX's rather crappy imitation one. }
procedure ShowModalAlert(const Msg: string; Style: NSAlertStyle = NSInformationalAlertStyle);
var
  Alert: NSAlert;
begin
  Alert := TNSAlert.Create;
  try
    Alert.setMessageText(NSSTR(Msg));
    Alert.setAlertStyle(Style);
    Alert.runModal;
  finally
    Alert.release;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  InstallApplicationDelegate2(OpenFile);
  itmOpen.ShortCut := scCommand or Ord('O');
  itmExport.ShortCut := scCommand or scShift or Ord('S');
  FLoadedFiles := TObjectDictionary<string, TLoadedFile>.Create([doOwnsValues]);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FLoadedFiles.Free;
end;

procedure TfrmMain.DragEnter(const Data: TDragObject; const Point: TPointF);
begin
  { The default behaviour is to pass the event on to the control being hovered
    over. Handling the drag operation at the form level just means it doesn't
    matter what control files are dropped onto. }
  FHandleDragDirectly := (Data.Files <> nil);
  if not FHandleDragDirectly then inherited;
end;

procedure TfrmMain.DragOver(const Data: TDragObject; const Point: TPointF;
  var Accept: Boolean);
begin
  if FHandleDragDirectly then
    Accept := True
  else
    inherited;
end;

procedure TfrmMain.DragDrop(const Data: TDragObject; const Point: TPointF);
var
  S: string;
begin
  if FHandleDragDirectly then
    for S in Data.Files do
      OpenFile(S)
  else
    inherited;
end;

procedure TfrmMain.itmOpenClick(Sender: TObject);
begin
  if dlgOpen.Execute then OpenFile(dlgOpen.FileName);
end;

procedure TfrmMain.itmExportClick(Sender: TObject);
begin
  if not dlgSave.Execute then Exit;
  memDisplay.Lines.SaveToFile(dlgSave.FileName);
  ShowModalAlert('Exported to ' + dlgSave.FileName);
end;

procedure TfrmMain.lsbFilesChange(Sender: TObject);
var
  SelItem: TListBoxItem;
  LoadedFile: TLoadedFile;
begin
  SelItem := lsbFiles.Selected;
  if SelItem = nil then Exit;
  LoadedFile := SelItem.TagObject as TLoadedFile;
  lblFileName.Text := LoadedFile.FileName;
  memDisplay.Text := LoadedFile.Contents;
end;

procedure TfrmMain.CloseButtonClick(Sender: TObject);
var
  Index: Integer;
  LoadedFile: TLoadedFile;
begin
  LoadedFile := (Sender as TControl).TagObject as TLoadedFile;
  Index := LoadedFile.ListItem.Index;
  if Index > 0 then
    lsbFiles.ItemIndex := Index - 1
  else
    if lsbFiles.Count > 1 then
      lsbFiles.ItemIndex := 1
    else
    begin
      lsbFiles.ItemIndex := -1;
      lblFileName.Text := '';
      memDisplay.Text := '';
    end;
  LoadedFile.ListItem.Free;
  FLoadedFiles.Remove(LoadedFile.FileName);
end;

procedure TfrmMain.OpenFile(const AFileName: string);
var
  CloseButton: TControl;
  LoadedFile: TLoadedFile;
begin
  //have we already loaded this file?
  if FLoadedFiles.TryGetValue(AFileName, LoadedFile) then
  begin
    lsbFiles.ItemIndex := LoadedFile.ListItem.Index;
    Exit;
  end;
  //no, so load it
  LoadedFile := TLoadedFile.Create;
  try
    LoadedFile.Contents := TFile.ReadAllText(AFileName);
    LoadedFile.FileName := AFileName;
    LoadedFile.ListItem := TListBoxItem.Create(Self);
    LoadedFile.ListItem.TagObject := LoadedFile;
    LoadedFile.ListItem.Text := ExtractFileName(AFileName);
    //add a close button to the list item
    CloseButton := CloseButtonTemplate.Clone(LoadedFile.ListItem) as TControl;
    CloseButton.Align := TAlignLayout.alRight;
    CloseButton.TagObject := LoadedFile;
    CloseButton.Visible := True;
    CloseButton.OnClick := CloseButtonClick;
    LoadedFile.ListItem.AddObject(CloseButton);
    CloseButton.Width := CloseButton.Height + 1;
    lsbFiles.AddObject(LoadedFile.ListItem);
  except
    LoadedFile.ListItem.Free;
    LoadedFile.Free;
    raise;
  end;
  FLoadedFiles.Add(AFileName, LoadedFile);
  lsbFiles.ItemIndex := lsbFiles.Count - 1;
  itmExport.Enabled := True;
end;

end.

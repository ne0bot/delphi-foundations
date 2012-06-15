unit MainForm;
{
  Demonstrates TCustomIniFile polymorphism. Data is loaded and saved by the
  LoadedSectionsClasses unit, which does not care about the particular
  descendant of TCustomIniFile being used. (Well, almost - identifiers get given
  an underscore prefix if the class has 'xml' in its name, and the section or
  key identifier passed in doesn't have a valid XML starting character.) The
  demo enforces this agnosticism by decoupling the UI and data with a mixture of
  'owner data' list views, TAction components, and communication with the main
  form being done via a custom interface type.

  For loading data, the TMemIniFile, TRegistryIniFile and TXmlIniFile classes
  are used; for saving data, TMemIniFile and TXmlIniFile. TRegistryIniFile isn't
  used for output because data types can be important in a Registry context, and
  the TCustomIniFile interface does not enable you to learn what is the most
  appropriate type for a given key. When it's your own data that's no
  problem (if the MagicNum key should have an integral value, then just call
  WriteInteger), but it does make generic conversions between different backing
  store types problematic.
}
interface

uses
  System.Types, System.UITypes, System.SysUtils, System.Classes, System.IniFiles,
  Vcl.ActnList, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  Vcl.ToolWin, Vcl.ImgList, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus,
  LoadedSectionsClasses, AppActions;

type
  TfrmMain = class(TForm, IDataView)
    ToolBar: TToolBar;
    btnLoad: TToolButton;
    btnSave: TToolButton;
    panSections: TPanel;
    lsvSections: TListView;
    panKeys: TPanel;
    lsvKeys: TListView;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
    sep1: TToolButton;
    mnuLoad: TPopupMenu;
    itmLoadFromIniFile: TMenuItem;
    itmLoadFromRegistry: TMenuItem;
    itmLoadFromXMLFile: TMenuItem;
    mnuSave: TPopupMenu;
    itmSaveAsIniFile: TMenuItem;
    itmSaveAsXML: TMenuItem;
    sep2: TToolButton;
    btnAddSection: TToolButton;
    btnDeleteSection: TToolButton;
    btnAddKey: TToolButton;
    btnDeleteKey: TToolButton;
    btnRenameSection: TToolButton;
    btnEditKey: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure lsvSectionsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lsvKeysDblClick(Sender: TObject);
    procedure lsvKeysKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lsvSectionsDblClick(Sender: TObject);
    procedure lsvSectionsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lsvSectionsData(Sender: TObject; Item: TListItem);
    procedure lsvSectionsDataFind(Sender: TObject; Find: TItemFind;
      const FindString: string; const FindPosition: TPoint; FindData: Pointer;
      StartIndex: Integer; Direction: TSearchDirection; Wrap: Boolean;
      var Index: Integer);
    procedure lsvKeysDataFind(Sender: TObject; Find: TItemFind;
      const FindString: string; const FindPosition: TPoint; FindData: Pointer;
      StartIndex: Integer; Direction: TSearchDirection; Wrap: Boolean;
      var Index: Integer);
    procedure lsvKeysData(Sender: TObject; Item: TListItem);
  protected
    procedure MenuItemClick(Sender: TObject);
    { IDataView }
    function GetActiveSectionIndex: Integer;
    function GetActiveKeyIndex: Integer;
    procedure DataChanged(ChangeKind: TLoadedSectionsChangeKind; Data: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

uses System.Math;

{$R *.dfm}

resourcestring
  SStatusText = 'Source class: %s   Source path: %s';

procedure TfrmMain.FormCreate(Sender: TObject);
var
  Item: TMenuItem;
begin
  for Item in mnuLoad.Items do
  begin
    Item.Tag := NativeInt(btnLoad);
    Item.OnClick := MenuItemClick;
  end;
  for Item in mnuSave.Items do
  begin
    Item.Tag := NativeInt(btnSave);
    Item.OnClick := MenuItemClick;
  end;
  dtmActions.Sections.SetDataView(Self);
end;

procedure TfrmMain.lsvSectionsData(Sender: TObject; Item: TListItem);
begin
  Item.Caption := dtmActions.Sections[Item.Index].Name;
end;

procedure TfrmMain.lsvSectionsDataFind(Sender: TObject; Find: TItemFind;
  const FindString: string; const FindPosition: TPoint; FindData: Pointer;
  StartIndex: Integer; Direction: TSearchDirection; Wrap: Boolean;
  var Index: Integer);
begin
  Index := dtmActions.Sections.FindNearest(FindString, StartIndex, Wrap);
end;

procedure TfrmMain.lsvSectionsDblClick(Sender: TObject);
begin
  dtmActions.actRenameSection.Execute;
end;

procedure TfrmMain.lsvSectionsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    vkF2, vkReturn: dtmActions.actRenameSection.Execute;
    vkDelete: dtmActions.actDeleteSection.Execute;
  end;
end;

procedure TfrmMain.lsvSectionsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
  begin
    lsvKeys.Items.Count := dtmActions.Sections[lsvSections.ItemIndex].Keys.Count;
    lsvKeys.Invalidate;
  end
end;

procedure TfrmMain.lsvKeysData(Sender: TObject; Item: TListItem);
var
  Keys: TStrings;
begin
  Keys := dtmActions.Sections[lsvSections.ItemIndex].Keys;
  Item.Caption := Keys.Names[Item.Index];
  Item.SubItems.Add(Keys.ValueFromIndex[Item.Index]);
end;

procedure TfrmMain.lsvKeysDataFind(Sender: TObject; Find: TItemFind;
  const FindString: string; const FindPosition: TPoint; FindData: Pointer;
  StartIndex: Integer; Direction: TSearchDirection; Wrap: Boolean;
  var Index: Integer);
begin
  Index := dtmActions.Sections[lsvSections.ItemIndex].FindNearestKey(FindString,
    StartIndex, Wrap);
end;

procedure TfrmMain.lsvKeysDblClick(Sender: TObject);
begin
  dtmActions.actEditKey.Execute;
end;

procedure TfrmMain.lsvKeysKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    vkF2, vkReturn: dtmActions.actEditKey.Execute;
    vkDelete: dtmActions.actDeleteKey.Execute;
  end;
end;

procedure TfrmMain.MenuItemClick(Sender: TObject);
var
  Item: TMenuItem;
begin
  Item := (Sender as TMenuItem);
  Item.Action.Execute;
  TToolButton(Item.Tag).Action := Item.Action;
end;

{ IDataView implementation }

function TfrmMain.GetActiveSectionIndex: Integer;
begin
  Result := lsvSections.ItemIndex;
end;

function TfrmMain.GetActiveKeyIndex: Integer;
begin
  Result := lsvKeys.ItemIndex;
end;

procedure TfrmMain.DataChanged(ChangeKind: TLoadedSectionsChangeKind; Data: TObject);
var
  OldItemIndex: Integer;
  Section: TLoadedSection;
begin
  if ChangeKind = ckLoadedFromIni then
  begin
    lsvSections.Items.Count := dtmActions.Sections.Count;
    if dtmActions.Sections.Count > 0 then
    begin
      lsvSections.ItemIndex := -1; //make sure the next line causes our OnSelectItem handler to be called
      lsvSections.Items[0].Selected := True;
      lsvSections.Invalidate;
    end;
    StatusBar.SimpleText := Format(SStatusText,
      [Data.ClassName, (Data as TCustomIniFile).FileName]);
    Exit;
  end;
  Section := Data as TLoadedSection;
  case ChangeKind of
    ckAddedSection:
    begin
      lsvSections.Items.Count := Section.Owner.Count;
      lsvSections.Invalidate;
      lsvSections.ItemIndex := Section.Index;
      lsvKeys.Items.Count := 0;
    end;
    ckRemovingSection:
    begin
      OldItemIndex := lsvSections.ItemIndex;
      lsvSections.Items.Count := lsvSections.Items.Count - 1;
      if lsvSections.Items.Count = 0 then
        lsvKeys.Items.Count := 0
      else if Section.Index = OldItemIndex then
        lsvSections.ItemIndex := Max(OldItemIndex, lsvSections.Items.Count - 1);
    end;
    ckSectionName: lsvSections.Invalidate;
    ckSectionKeys:
    begin
      lsvKeys.Items.Count := Section.Keys.Count;
      lsvKeys.Invalidate;
    end;
  end;
end;

end.

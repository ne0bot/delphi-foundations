unit AppActions;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles, System.Generics.Collections,
  Winapi.Windows, Vcl.ActnList, Vcl.ImgList, Vcl.Controls, Vcl.Dialogs,
  LoadedSectionsClasses;

type
  TdtmActions = class(TDataModule)
    imlNormal: TImageList;
    ActionList: TActionList;
    actLoadFromIniFile: TAction;
    actLoadFromRegistry: TAction;
    actLoadFromXML: TAction;
    actSaveAsIniFile: TAction;
    dlgLoadFromIni: TOpenDialog;
    dlgLoadFromXml: TOpenDialog;
    actSaveAsXML: TAction;
    dlgSaveAsIni: TSaveDialog;
    dlgSaveAsXML: TSaveDialog;
    actAddSection: TAction;
    actDeleteSection: TAction;
    actRenameSection: TAction;
    actAddKey: TAction;
    actDeleteKey: TAction;
    actEditKey: TAction;
    imlDisabled: TImageList;
    procedure actLoadFromIniFileExecute(Sender: TObject);
    procedure actLoadFromRegistryExecute(Sender: TObject);
    procedure actSaveAsIniFileExecute(Sender: TObject);
    procedure actLoadFromXMLExecute(Sender: TObject);
    procedure actSaveAsXMLExecute(Sender: TObject);
    procedure actAddSectionExecute(Sender: TObject);
    procedure actDeleteSectionExecute(Sender: TObject);
    procedure actRenameSectionExecute(Sender: TObject);
    procedure actAddKeyExecute(Sender: TObject);
    procedure actEditKeyExecute(Sender: TObject);
    procedure actDeleteKeyExecute(Sender: TObject);
    procedure actActiveSectionUpdate(Sender: TObject);
    procedure actEditKeyUpdate(Sender: TObject);
  strict private
    FLastRegPath: string;
    FSections: TLoadedSections;
    FRegRootKeyMap: TDictionary<string,HKEY>;
    function CheckDuplicateSection(const AName: string): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Sections: TLoadedSections read FSections;
  end;

var
  dtmActions: TdtmActions;

implementation

uses
  System.Math, System.IOUtils, System.Win.Registry,
  Xml.XMLIntf, Xml.XmlDoc, Xml.XmlIniFile;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

resourcestring
  SConfirmDupSectionName = 'A section called "%s" already exists. Are you sure ' +
    'you want another section with the same name?';
  SConfirmDeleteSection = 'Are you sure you want to delete the "%s" section?';
  SConfirmDeleteKey = 'Are you sure you want to delete the selected key?';

{ TXmlIniFile is designed for use with an independently-managed IXmlDocument
  instance. This descendant class changes that. }
type
  TStandaloneXmlIniFile = class(TXmlIniFile)
  strict private
    FDocument: IXMLDocument;
  public
    constructor Create(const AFileName: string);
    procedure UpdateFile; override;
  end;

constructor TStandaloneXmlIniFile.Create(const AFileName: string);
begin
  if FileExists(AFileName) then
    FDocument := LoadXMLDocument(AFileName)
  else
  begin
    FDocument := NewXMLDocument;
    FDocument.Options := FDocument.Options + [doNodeAutoIndent];
  end;
  if FDocument.DocumentElement = nil then
    FDocument.DocumentElement := FDocument.CreateNode('Sections');
  TCustomIniFile(Self).Create(AFileName); //make sure our FileName property is set
  inherited Create(FDocument.DocumentElement);
end;

procedure TStandaloneXmlIniFile.UpdateFile;
begin
  FDocument.SaveToFile(FileName);
end;

{ TdtmActions }

constructor TdtmActions.Create(AOwner: TComponent);
begin
  inherited;
  FSections := TLoadedSections.Create;
  FLastRegPath := 'HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer';
  FRegRootKeyMap := TDictionary<string,HKEY>.Create;
  FRegRootKeyMap.Add('HKEY_CLASSES_ROOT', HKEY_CLASSES_ROOT);
  FRegRootKeyMap.Add('HKEY_CURRENT_USER', HKEY_CURRENT_USER);
  FRegRootKeyMap.Add('HKEY_LOCAL_MACHINE', HKEY_LOCAL_MACHINE);
  FRegRootKeyMap.Add('HKEY_USERS', HKEY_USERS);
  FRegRootKeyMap.Add('HKEY_PERFORMANCE_DATA', HKEY_PERFORMANCE_DATA);
  FRegRootKeyMap.Add('HKEY_CURRENT_CONFIG', HKEY_CURRENT_CONFIG);
  FRegRootKeyMap.Add('HKEY_DYN_DATA', HKEY_DYN_DATA);
end;

destructor TdtmActions.Destroy;
begin
  FRegRootKeyMap.Free;
  FSections.Free;
  inherited;
end;

function TdtmActions.CheckDuplicateSection(const AName: string): Boolean;
begin
  Result := (FSections.IndexOf(Name) < 0) or IsPositiveResult(MessageDlg(Format(
    SConfirmDupSectionName, [Name]), mtWarning, mbYesNo, 0));
end;

procedure TdtmActions.actAddSectionExecute(Sender: TObject);
var
  Name: string;
begin
  if not InputQuery('New Section', 'Name:', Name) or (Name = '') then Exit;
  if CheckDuplicateSection(Name) then FSections.AddAfterActive(Name);
end;

procedure TdtmActions.actDeleteSectionExecute(Sender: TObject);
begin
  FSections.RemoveActiveSection;
end;

procedure TdtmActions.actActiveSectionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FSections.DataView.GetActiveSectionIndex >= 0)
end;

procedure TdtmActions.actRenameSectionExecute(Sender: TObject);
var
  Index: Integer;
  Name: string;
  Section: TLoadedSection;
begin
  Index := FSections.DataView.GetActiveSectionIndex;
  if Index < 0 then Exit;
  Section := FSections[Index];
  Name := Section.Name;
  if not InputQuery('Rename Section', 'New name:', Name) or (Name = '') then Exit;
  if SameText(Name, Section.Name) or CheckDuplicateSection(Name) then
    Section.Name := Name;
end;

procedure TdtmActions.actAddKeyExecute(Sender: TObject);
var
  Data: array[0..1] of string;
  KeyIndex, SectionIndex: Integer;
begin
  SectionIndex := FSections.DataView.GetActiveSectionIndex;
  if SectionIndex < 0 then Exit;
  if not InputQuery('New Key', ['Name:', 'Value:'], Data) then Exit;
  KeyIndex := FSections.DataView.GetActiveKeyIndex;
  if KeyIndex < 0 then KeyIndex := FSections[SectionIndex].Keys.Count;
  with FSections[SectionIndex].Keys do
    Insert(KeyIndex, Data[0] + NameValueSeparator + Data[1]);
end;

procedure TdtmActions.actEditKeyExecute(Sender: TObject);
var
  Data: array[0..1] of string;
  Keys: TStrings;
  KeyIndex, SectionIndex: Integer;
begin
  SectionIndex := FSections.DataView.GetActiveSectionIndex;
  KeyIndex := FSections.DataView.GetActiveKeyIndex;
  if (SectionIndex < 0) or (KeyIndex < 0) then Exit;
  Keys := FSections[SectionIndex].Keys;
  Data[0] := Keys.Names[KeyIndex];
  Data[1] := Keys.ValueFromIndex[KeyIndex];
  if not InputQuery('Edit Key', ['New name:', 'New value:'], Data) then Exit;
  Keys[KeyIndex] := Data[0] + Keys.NameValueSeparator + Data[1];
end;

procedure TdtmActions.actEditKeyUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (FSections.DataView.GetActiveKeyIndex >= 0);
end;

procedure TdtmActions.actDeleteKeyExecute(Sender: TObject);
var
  KeyIndex, SectionIndex: Integer;
begin
  SectionIndex := FSections.DataView.GetActiveSectionIndex;
  KeyIndex := FSections.DataView.GetActiveKeyIndex;
  if (SectionIndex < 0) or (KeyIndex < 0) then Exit;
  if IsPositiveResult(MessageDlg(SConfirmDeleteKey, mtWarning, mbYesNo, 0)) then
    FSections[SectionIndex].Keys.Delete(KeyIndex);
end;

procedure TdtmActions.actLoadFromIniFileExecute(Sender: TObject);
var
  IniFile: TMemIniFile;
begin
  if not dlgLoadFromIni.Execute then Exit;
  IniFile := TMemIniFile.Create(dlgLoadFromIni.FileName);
  try
    Sections.LoadFromIniFile(IniFile);
  finally
    IniFile.Free;
  end;
end;

procedure TdtmActions.actLoadFromRegistryExecute(Sender: TObject);
var
  I: Integer;
  IniFile: TRegistryIniFile;
  RootKey: HKEY;
  RootKeyStr: string;
begin
  if not InputQuery('Open From Registry', 'Registry path to read from:', FLastRegPath) then Exit;
  I := Pos('\', FLastRegPath);
  if I = 0 then raise EProgrammerNotFound.Create('Invalid path specification - expected at least one backslash');
  RootKeyStr := UpperCase(Copy(FLastRegPath, 1, I - 1));
  if not FRegRootKeyMap.TryGetValue(RootKeyStr, RootKey) then
    raise EProgrammerNotFound.CreateFmt('Invalid root key (%s)', [RootKeyStr]);
  IniFile := TRegistryIniFile.Create(Copy(FLastRegPath, I, MaxInt), KEY_READ);
  try
    if RootKey <> HKEY_CURRENT_USER then
    begin
      IniFile.RegIniFile.RootKey := RootKey;
      IniFile.RegIniFile.OpenKey(IniFile.FileName, False);
    end;
    Sections.LoadFromIniFile(IniFile);
  finally
    IniFile.Free;
  end;
end;

procedure TdtmActions.actLoadFromXMLExecute(Sender: TObject);
var
  IniFile: TStandaloneXmlIniFile;
begin
  if not dlgLoadFromXml.Execute then Exit;
  IniFile := TStandaloneXmlIniFile.Create(dlgLoadFromXML.FileName);
  try
    Sections.LoadFromIniFile(IniFile);
  finally
    IniFile.Free;
  end;
end;

procedure TdtmActions.actSaveAsIniFileExecute(Sender: TObject);
var
  IniFile: TMemIniFile;
begin
  if not dlgSaveAsIni.Execute then Exit;
  TFile.Delete(dlgSaveAsIni.FileName); //clear whatever was there before
  IniFile := TMemIniFile.Create(dlgSaveAsIni.FileName, TEncoding.UTF8);
  try
    Sections.SaveToIniFile(IniFile);
  finally
    IniFile.Free;
  end;
end;

procedure TdtmActions.actSaveAsXMLExecute(Sender: TObject);
var
  IniFile: TStandaloneXmlIniFile;
begin
  if not dlgSaveAsXML.Execute then Exit;
  TFile.Delete(dlgSaveAsXML.FileName); //clear whatever was there before
  IniFile := TStandaloneXmlIniFile.Create(dlgSaveAsXML.FileName);
  try
    Sections.SaveToIniFile(IniFile);
  finally
    IniFile.Free;
  end;
end;

end.

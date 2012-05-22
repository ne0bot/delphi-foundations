unit LoadedSectionsClasses;
{
  Implements a simple data structure to hold loaded section information, together
  with methods to load from and write to an arbitrary TCustomIniFile descendant.
}
interface

uses
  System.SysUtils, System.Classes, System.IniFiles, System.Generics.Collections;

type
  TLoadedSection = class;
  TLoadedSections = class;

  TLoadedSectionsChangeKind = (ckLoadedFromIni, ckAddedSection, ckRemovingSection,
    ckSectionName, ckSectionKeys);

  IDataView = interface
    ['{7312B540-E24A-4CE8-97C9-5AF2B2CC6FC6}']
    function GetActiveSectionIndex: Integer;
    function GetActiveKeyIndex: Integer;
    procedure DataChanged(ChangeKind: TLoadedSectionsChangeKind; Data: TObject);
  end;

  TLoadedSection = class
  strict private
    FName: string;
    FKeys: TStrings;
    FOwner: TLoadedSections;
    function GetIndex: Integer;
    procedure SetName(const Value: string);
  public
    constructor Create(AOwner: TLoadedSections; const AName: string);
    destructor Destroy; override;
    function FindNearestKey(const AStartOfName: string; AStartIndex: Integer;
      AWrap: Boolean): Integer;
    property Index: Integer read GetIndex;
    property Name: string read FName write SetName;
    property Keys: TStrings read FKeys;
    property Owner: TLoadedSections read FOwner;
  end;

  TLoadedSections = class(TEnumerable<TLoadedSection>)
  strict private
    FDataView: IDataView;
    FLoading: Boolean;
    FItems: TObjectList<TLoadedSection>;
    function GetItem(Index: Integer): TLoadedSection;
    function GetCount: Integer;
  protected
    function DoGetEnumerator: TEnumerator<TLoadedSection>; override;
    procedure DoChange(Kind: TLoadedSectionsChangeKind; Data: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AName: string): TLoadedSection; inline;
    function AddAfterActive(const AName: string): TLoadedSection;
    procedure Delete(AIndex: Integer);
    procedure RemoveActiveSection;
    function FindNearest(const AStartOfName: string; AStartIndex: Integer;
      AWrap: Boolean): Integer;
    function Insert(AIndex: Integer; const AName: string): TLoadedSection;
    function IndexOf(ASection: TLoadedSection): Integer; overload; inline;
    function IndexOf(const ASectionName: string): Integer; overload;
    procedure LoadFromIniFile(IniFile: TCustomIniFile);
    procedure SaveToIniFile(IniFile: TCustomIniFile);
    procedure SetDataView(const ADataView: IDataView);
    property Count: Integer read GetCount;
    property DataView: IDataView read FDataView;
    property Items[Index: Integer]: TLoadedSection read GetItem; default;
  end;

implementation

uses
  System.StrUtils;

type
  TKeys = class(TStringList)
  strict private
    FOwner: TLoadedSection;
  protected
    procedure Changed; override;
    procedure Put(Index: Integer; const S: string); override;
  public
    constructor Create(AOwner: TLoadedSection);
  end;

constructor TKeys.Create(AOwner: TLoadedSection);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TKeys.Put(Index: Integer; const S: string);
begin
  if Strings[Index] <> S then inherited; //avoid Changed being called when nothing has changed
end;

procedure TKeys.Changed;
begin
  FOwner.Owner.DoChange(ckSectionKeys, FOwner);
  inherited;
end;

{ TLoadedSection }

constructor TLoadedSection.Create(AOwner: TLoadedSections; const AName: string);
begin
  inherited Create;
  FKeys := TKeys.Create(Self);
  FName := AName;
  FOwner := AOwner;
end;

destructor TLoadedSection.Destroy;
begin
  FKeys.Free;
  inherited;
end;

function TLoadedSection.FindNearestKey(const AStartOfName: string;
  AStartIndex: Integer; AWrap: Boolean): Integer;
begin
  for Result := AStartIndex to FKeys.Count - 1 do
    if StartsText(AStartOfName, FKeys.Names[Result]) then Exit;
  if AWrap then
    for Result := 0 to AStartIndex - 1 do
      if StartsText(AStartOfName, FKeys.Names[Result]) then Exit;
  Result := -1;
end;

function TLoadedSection.GetIndex: Integer;
begin
  Result := FOwner.IndexOf(Self)
end;

procedure TLoadedSection.SetName(const Value: string);
begin
  if FName = Value then Exit;
  FName := Value;
  FOwner.DoChange(ckSectionName, Self);
end;

{ TLoadedSections }

constructor TLoadedSections.Create;
begin
  inherited Create;
  FItems := TObjectList<TLoadedSection>.Create;
end;

destructor TLoadedSections.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TLoadedSections.DoGetEnumerator: TEnumerator<TLoadedSection>;
begin
  Result := FItems.GetEnumerator;
end;

function TLoadedSections.FindNearest(const AStartOfName: string;
  AStartIndex: Integer; AWrap: Boolean): Integer;
begin
  for Result := AStartIndex to FItems.Count - 1 do
    if StartsText(AStartOfName, FItems[Result].Name) then Exit;
  if AWrap then
    for Result := 0 to AStartIndex - 1 do
      if StartsText(AStartOfName, FItems[Result].Name) then Exit;
  Result := -1;
end;

function TLoadedSections.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TLoadedSections.GetItem(Index: Integer): TLoadedSection;
begin
  Result := FItems[Index];
end;

function TLoadedSections.Add(const AName: string): TLoadedSection;
begin
  Result := Insert(FItems.Count, AName)
end;

function TLoadedSections.AddAfterActive(const AName: string): TLoadedSection;
var
  Index: Integer;
begin
  if FDataView <> nil then
    Index := FDataView.GetActiveSectionIndex + 1
  else
    Index := 0;
  if Index <= 0 then Index := FItems.Count;
  Result := Insert(Index, AName)
end;

procedure TLoadedSections.Delete(AIndex: Integer);
var
  Item: TLoadedSection;
begin
  Item := FItems[AIndex];
  DoChange(ckRemovingSection, Item);
  FItems.Delete(AIndex);
end;

procedure TLoadedSections.RemoveActiveSection;
var
  Index: Integer;
begin
  if FDataView = nil then Exit;
  Index := FDataView.GetActiveSectionIndex;
  if Index >= 0 then Delete(Index);
end;

function TLoadedSections.Insert(AIndex: Integer; const AName: string): TLoadedSection;
begin
  Result := TLoadedSection.Create(Self, AName);
  try
    FItems.Insert(AIndex, Result);
  except
    Result.Free;
    raise;
  end;
  DoChange(ckAddedSection, Result);
end;

function TLoadedSections.IndexOf(ASection: TLoadedSection): Integer;
begin
  Result := FItems.IndexOf(ASection);
end;

function TLoadedSections.IndexOf(const ASectionName: string): Integer;
begin
  for Result := 0 to FItems.Count - 1 do
    if SameText(FItems[Result].Name, ASectionName) then Exit;
  Result := -1;
end;

procedure TLoadedSections.DoChange(Kind: TLoadedSectionsChangeKind; Data: TObject);
begin
  if not FLoading and Assigned(FDataView) then
    FDataView.DataChanged(Kind, Data);
end;

procedure TLoadedSections.LoadFromIniFile(IniFile: TCustomIniFile);
var
  NewItem: TLoadedSection;
  S: string;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    FLoading := True;
    IniFile.ReadSections(Strings);
    FItems.Clear;
    for S in Strings do
    begin
      NewItem := Add(S);
      IniFile.ReadSectionValues(S, NewItem.Keys);
    end;
  finally
    Strings.Free;
    FLoading := False;
  end;
  if Assigned(FDataView) then FDataView.DataChanged(ckLoadedFromIni, IniFile);
end;

procedure TLoadedSections.SaveToIniFile(IniFile: TCustomIniFile);
var
  ToXML: Boolean;

  function PrefixIfNec(const AName: string): string;
  begin
    if ToXML and ((AName = '') or CharInSet(AName[1], ['0'..'9', '.', '-'])) then
      Result := '_' + AName
    else
      Result := AName;
  end;
var
  I: Integer;
  Section: TLoadedSection;
  S: string;
begin
  ToXML := TextPos(PChar(IniFile.ClassName), 'Xml') <> nil;
  for Section in Self do
  begin
    S := PrefixIfNec(Section.Name);
    for I := 0 to Section.Keys.Count - 1 do
      IniFile.WriteString(S, PrefixIfNec(Section.Keys.Names[I]), Section.Keys.ValueFromIndex[I]);
  end;
  IniFile.UpdateFile;
end;

procedure TLoadedSections.SetDataView(const ADataView: IDataView);
begin
  FDataView := ADataView;
end;

end.

program Attempt3;
{
  Another fixed version of Attempt1.dpr. This time, we define a custom
  streamable list class rather than override DefineProperties.
}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes;

type
  TPeopleList = class;

  TPerson = class(TCollectionItem)
  private
    FSurname: string;
    FForename: string;
  published
    property Forename: string read FForename write FForename;
    property Surname: string read FSurname write FSurname;
  end;

  TPeopleList = class(TOwnedCollection)
  public type
    TEnumerator = record
    strict private
      FList: TPeopleList;
      FIndex: Integer;
      function GetCurrent: TPerson;
    public
      constructor Create(AList: TPeopleList);
      function MoveNext: Boolean;
      property Current: TPerson read GetCurrent;
    end;
  strict private
    function GetItem(Index: Integer): TPerson;
  public
    constructor Create(AOwner: TPersistent);
    function GetEnumerator: TEnumerator;
    function Add: TPerson; inline;
    property Items[Index: Integer]: TPerson read GetItem; default;
  end;

  TVisitData = class(TComponent)
  strict private
    FDescription: string;
    FPeople: TPeopleList;
    FPlaces: TStrings;
    procedure SetPeople(AValue: TPeopleList);
    procedure SetPlaces(AValue: TStrings);
  public
    constructor Create(AOwner: TComponent = nil); override;
    destructor Destroy; override;
  published
    property Description: string read FDescription write FDescription;
    property People: TPeopleList read FPeople write SetPeople;
    property Places: TStrings read FPlaces write SetPlaces;
  end;

{ TPeopleList.TEnumerator }

constructor TPeopleList.TEnumerator.Create(AList: TPeopleList);
begin
  FList := AList;
  FIndex := -1;
end;

function TPeopleList.TEnumerator.GetCurrent: TPerson;
begin
  Result := FList[FIndex];
end;

function TPeopleList.TEnumerator.MoveNext: Boolean;
begin
  Result := (Succ(FIndex) < FList.Count);
  if Result then Inc(FIndex);
end;

{ TPeopleList }

constructor TPeopleList.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TPerson);
end;

function TPeopleList.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TPeopleList.GetItem(Index: Integer): TPerson;
begin
  Result := TPerson(inherited GetItem(Index));
end;

function TPeopleList.Add: TPerson;
begin
  Result := TPerson(inherited Add);
end;

{ TVisitData }

constructor TVisitData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPeople := TPeopleList.Create(Self);
  FPlaces := TStringList.Create;
end;

destructor TVisitData.Destroy;
begin
  FPlaces.Free;
  FPeople.Free;
  inherited;
end;

procedure TVisitData.SetPeople(AValue: TPeopleList);
begin
  FPeople.Assign(AValue);
end;

procedure TVisitData.SetPlaces(AValue: TStrings);
begin
  FPlaces.Assign(AValue);
end;

procedure OutputData(ABinaryStream: TStream);
const
  UTF8BOM: array[1..3] of AnsiChar = (#$EF, #$BB, #$BF);
var
  TextStream: TStringStream;
begin
  ABinaryStream.Position := 0;
  TextStream := TStringStream.Create('', TEncoding.UTF8);
  try
    ObjectBinaryToText(ABinaryStream, TextStream);
    if CompareMem(TextStream.Memory, @UTF8BOM, SizeOf(UTF8BOM)) then
      TextStream.Position := SizeOf(UTF8BOM)
    else
      TextStream.Position := 0;
    WriteLn(TextStream.DataString);
  finally
    TextStream.Free;
  end;
end;

var
  Person: TPerson;
  Place: string;
  Root: TVisitData;
  Stream: TMemoryStream;
begin
  Stream := nil;
  Root := TVisitData.Create;
  try
    Root.Description := 'This should now work!';
    Root.Places.Add('Dorset');
    Root.Places.Add('Somerset');
    Person := Root.People.Add;
    Person.Forename := 'John';
    Person.Surname := 'Smith';
    //save the root object to the stream, and output a text version to the screen
    Stream := TMemoryStream.Create;
    Stream.WriteComponent(Root);
    OutputData(Stream);
    //recreate the root object afresh, and load it from the stream
    FreeAndNil(Root);
    Root := TVisitData.Create;
    Stream.Position := 0;
    Stream.ReadComponent(Root);
    WriteLn('Description: ', Root.Description);
    WriteLn('No. of people: ', Root.People.Count);
    if Root.People.Count > 0 then
      for Person in Root.People do
        WriteLn('  ' + Person.Forename + ' ' + Person.Surname);
    WriteLn('No. of places: ', Root.Places.Count);
    if Root.Places.Count > 0 then
      for Place in Root.Places do
        WriteLn('  ' + Place);
  finally
    Root.Free;
    Stream.Free;
  end;
  ReadLn;
end.

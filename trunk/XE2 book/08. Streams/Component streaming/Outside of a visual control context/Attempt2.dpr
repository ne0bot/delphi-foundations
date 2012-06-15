program Attempt2;
{
  Fixed version of Attempt1.dpr:
  - The Description property is made streamable simply by giving it published
    visibility.
  - The Places property is made streamable by converting it to use TStrings/
    TStringList, giving it a setter, and publishing it.
  - The People property is persisted by overriding the root object's
    DefineProperties method and writing out the object list manually.
}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections;

type
  TPerson = class
  strict private
    FSurname: string;
    FForename: string;
  public
    property Forename: string read FForename write FForename;
    property Surname: string read FSurname write FSurname;
  end;

  TVisitData = class(TComponent)
  strict private
    FDescription: string;
    FPeople: TObjectList<TPerson>;
    FPlaces: TStrings;
    procedure ReadPeople(Reader: TReader);
    procedure WritePeople(Writer: TWriter);
    procedure SetPlaces(AValue: TStrings);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent = nil); override;
    destructor Destroy; override;
    property People: TObjectList<TPerson> read FPeople;
  published
    property Description: string read FDescription write FDescription;
    property Places: TStrings read FPlaces write SetPlaces;
  end;

{ TVisitData }

constructor TVisitData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPeople := TObjectList<TPerson>.Create;
  FPlaces := TStringList.Create;
end;

destructor TVisitData.Destroy;
begin
  FPlaces.Free;
  FPeople.Free;
  inherited;
end;

procedure TVisitData.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('People', ReadPeople, WritePeople, FPeople.Count > 0);
end;

procedure TVisitData.ReadPeople(Reader: TReader);
var
  NewPerson: TPerson;
begin
  Reader.ReadListBegin;
  FPeople.Clear;
  while not Reader.EndOfList do
  begin
    NewPerson := TPerson.Create;
    try
      NewPerson.Forename := Reader.ReadString;
      NewPerson.Surname := Reader.ReadString;
      FPeople.Add(NewPerson);
    except
      NewPerson.Free;
      raise;
    end;
  end;
  Reader.ReadListEnd;
end;

procedure TVisitData.WritePeople(Writer: TWriter);
var
  Person: TPerson;
begin
  Writer.WriteListBegin;
  for Person in FPeople do
  begin
    Writer.WriteString(Person.Forename);
    Writer.WriteString(Person.Surname);
  end;
  Writer.WriteListEnd;
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
    Person := TPerson.Create;
    Person.Forename := 'John';
    Person.Surname := 'Smith';
    Root.People.Add(Person);
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

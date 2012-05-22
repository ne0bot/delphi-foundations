program Attempt1;
{
  Naive attempt at using component streaming outside its usual context of
  persisting property settings of visual controls. Since the mechanics of
  component streaming are not taken heed of, this example will *not* work!
  See Attempt2.dpr and Attempt3.dpr for working versions.
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
    FPlaces: TList<string>;
  public
    constructor Create(AOwner: TComponent = nil); override;
    destructor Destroy; override;
    property Description: string read FDescription write FDescription;
    property People: TObjectList<TPerson> read FPeople;
    property Places: TList<string> read FPlaces;
  end;

constructor TVisitData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPeople := TObjectList<TPerson>.Create;
  FPlaces := TList<string>.Create;
end;

destructor TVisitData.Destroy;
begin
  FPlaces.Free;
  FPeople.Free;
  inherited;
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
    Root.Description := 'This will not actually work!';
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

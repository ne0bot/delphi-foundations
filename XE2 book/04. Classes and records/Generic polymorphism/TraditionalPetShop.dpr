program TraditionalPetShop;
{
  Creates a type safe list the old way, i.e. non-generically. Should be compared with the code for
  GenericPetShop.
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Contnrs, //for the legacy non-generic TObjectList
  PetShop.Pets in 'PetShop.Pets.pas';

type
  TAnimalList = class(TObjectList)
  public type
    TEnumerator = record
    strict private
      FCurrentIndex: Integer;
      FSource: TAnimalList;
      function GetCurrent: TAnimal;
    public
      constructor Create(ASource: TAnimalList);
      function MoveNext: Boolean;
      property Current: TAnimal read GetCurrent;
    end;
  strict private
    function GetItem(Index: Integer): TAnimal;
    procedure SetItem(Index: Integer; Value: TAnimal);
  public
    function GetEnumerator: TEnumerator;
    function Add(Item: TAnimal): Integer; inline;
    function Extract(Item: TAnimal): TAnimal; inline;
    function First: TAnimal; inline;
    function Last: TAnimal; inline;
    property Items[Index: Integer]: TAnimal read GetItem write SetItem; default;
    //finally, we can add our custom method!
    procedure AllMakeSound;
  end;

  TPuppyList = class(TAnimalList)
  public type
    TEnumerator = record
    strict private
      FCurrentIndex: Integer;
      FSource: TPuppyList;
      function GetCurrent: TPuppy;
    public
      constructor Create(ASource: TPuppyList);
      function MoveNext: Boolean;
      property Current: TPuppy read GetCurrent;
    end;
  strict private
    function GetItem(Index: Integer): TPuppy;
    procedure SetItem(Index: Integer; Value: TPuppy);
  public
    function GetEnumerator: TEnumerator;
    function Add(Item: TPuppy): Integer; inline;
    function Extract(Item: TPuppy): TPuppy; inline;
    function First: TPuppy; inline;
    function Last: TPuppy; inline;
    property Items[Index: Integer]: TPuppy read GetItem write SetItem; default;
    //once more, we can finally add our custom method...
    function HighestPedigreeRating: TPedigreeRating;
  end;

{ reintroduced methods - TAnimalList }

constructor TAnimalList.TEnumerator.Create(ASource: TAnimalList);
begin
  FCurrentIndex := -1;
  FSource := ASource;
end;

function TAnimalList.TEnumerator.GetCurrent: TAnimal;
begin
  Result := FSource[FCurrentIndex];
end;

function TAnimalList.TEnumerator.MoveNext: Boolean;
begin
  Result := (FCurrentIndex < Pred(FSource.Count));
  if Result then Inc(FCurrentIndex);
end;

function TAnimalList.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TAnimalList.Add(Item: TAnimal): Integer;
begin
  Result := inherited Add(Item);
end;

function TAnimalList.Extract(Item: TAnimal): TAnimal;
begin
  Result := TAnimal(inherited Extract(Item));
end;

function TAnimalList.First: TAnimal;
begin
  Result := TAnimal(inherited First);
end;

function TAnimalList.Last: TAnimal;
begin
  Result := TAnimal(inherited Last);
end;

function TAnimalList.GetItem(Index: Integer): TAnimal;
begin
  Result := TAnimal(inherited Items[Index]);
end;

procedure TAnimalList.SetItem(Index: Integer; Value: TAnimal);
begin
  inherited Items[Index] := Value;
end;

{ reintroduced methods - TPuppyList }

constructor TPuppyList.TEnumerator.Create(ASource: TPuppyList);
begin
  FCurrentIndex := -1;
  FSource := ASource;
end;

function TPuppyList.TEnumerator.GetCurrent: TPuppy;
begin
  Result := FSource[FCurrentIndex];
end;

function TPuppyList.TEnumerator.MoveNext: Boolean;
begin
  Result := (FCurrentIndex < Pred(FSource.Count));
  if Result then Inc(FCurrentIndex);
end;

function TPuppyList.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TPuppyList.Add(Item: TPuppy): Integer;
begin
  Result := inherited Add(Item);
end;

function TPuppyList.Extract(Item: TPuppy): TPuppy;
begin
  Result := TPuppy(inherited Extract(Item));
end;

function TPuppyList.First: TPuppy;
begin
  Result := TPuppy(inherited First);
end;

function TPuppyList.Last: TPuppy;
begin
  Result := TPuppy(inherited Last);
end;

function TPuppyList.GetItem(Index: Integer): TPuppy;
begin
  Result := TPuppy(inherited Items[Index]);
end;

procedure TPuppyList.SetItem(Index: Integer; Value: TPuppy);
begin
  inherited Items[Index] := Value;
end;

{ our custom methods }

procedure TAnimalList.AllMakeSound;
var
  Animal: TAnimal;
begin
  for Animal in Self do
    Animal.MakeSound;
end;

function TPuppyList.HighestPedigreeRating: TPedigreeRating;
var
  Puppy: TPuppy;
begin
  Result := prLow;
  for Puppy in Self do
    if Puppy.PedigreeRating > Result then Result := Puppy.PedigreeRating;
end;

procedure FeedAnimals(List: TAnimalList);
begin
  Writeln('Feeding the animals...');
  List.AllMakeSound;
  Writeln('Urgh, maybe later');
end;

procedure MoveAnimals(FromList, ToList: TAnimalList);
var
  I: Integer;
  SavedOwnsObjects: Boolean;
begin
  SavedOwnsObjects := FromList.OwnsObjects;
  FromList.OwnsObjects := False;
  try
    for I := FromList.Count - 1 downto 0 do
    begin
      ToList.Add(FromList[I]);
      FromList.Delete(I);
    end;
  finally
    FromList.OwnsObjects := SavedOwnsObjects;
  end;
end;

var
  Animals: TAnimalList;
  Puppies: TPuppyList;
begin
  Animals := nil;
  Puppies := TPuppyList.Create;
  try
    //add some puppies to the puppy list, and find the highest pedigree rating
    Puppies.Add(TPuppy.Create(prLow));
    Puppies.Add(TPuppy.Create(prMedium));
    Puppies.Add(TPuppy.Create(prHigh));
    WriteLn('Highest pedigree rating is: ',
      PedigreeRatingStrings[Puppies.HighestPedigreeRating]);
    //feed the puppies via our utility routine
    FeedAnimals(Puppies);
    Writeln;
    //create the animal list, add a piranha, a snake, the puppies, and another snake
    Animals := TAnimalList.Create;
    Animals.Add(TPiranha.Create);
    Animals.Add(TSnake.Create);
    MoveAnimals(Puppies, Animals);
    Animals.Add(TSnake.Create);
    //feed the animals again
    FeedAnimals(Animals);
  finally
    Puppies.Free;
    Animals.Free;
  end;
  Readln;
end.

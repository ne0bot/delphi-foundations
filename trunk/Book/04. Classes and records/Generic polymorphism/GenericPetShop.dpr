program GenericPetShop;
{
  Generics-using version of TraditionalPetShop, which allows the code to be cut down
  considerably when it comes to defining the custom list classes (imagine doing further
  list classes for the other animal types too...). Demonstrates the principle of keeping 
  the generic 'open' - notice the Feed and MoveTo calls both require type arguments.
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Generics.Collections,
  PetShop.Pets in 'PetShop.Pets.pas';

type
  TAnimalList<T: TAnimal> = class(TObjectList<T>)
    procedure AllMakeSound;
    procedure MoveTo<DestElemType: TAnimal>(List: TAnimalList<DestElemType>); overload;
    procedure MoveTo(List: TAnimalList<T>); overload;
  end;

  TAnimalList = TAnimalList<TAnimal>; //Defined only for convenience
  
  TPuppyList<T: TPuppy> = class(TAnimalList<TPuppy>)
    function HighestPedigreeRating: TPedigreeRating;
  end;

  TPuppyList = TPuppyList<TPuppy>;    //Defined only for convenience.

  TAnimalUtils = record               //Can't define generic standalone routines, so use static method on a record type instead
    class procedure Feed<ElemType: TAnimal>(List: TAnimalList<ElemType>); static;
  end;

procedure TAnimalList<T>.AllMakeSound;
var
  Animal: TAnimal;
begin
  for Animal in Self do
    Animal.MakeSound;
end;

procedure TAnimalList<T>.MoveTo(List: TAnimalList<T>);
begin
  MoveTo<T>(List);
end;

procedure TAnimalList<T>.MoveTo<DestElemType>(List: TAnimalList<DestElemType>);
var
  I: Integer;
  SavedOwnsObjects: Boolean;
begin
  SavedOwnsObjects := OwnsObjects;
  OwnsObjects := False;
  try
    for I := Count - 1 downto 0 do
    begin
      List.Add(Items[I] as DestElemType);
      Delete(I);
    end;
  finally
    OwnsObjects := SavedOwnsObjects;
  end;
end;

function TPuppyList<T>.HighestPedigreeRating: TPedigreeRating;
var
  Puppy: TPuppy;
begin
  Result := prLow;
  for Puppy in Self do
    if Puppy.PedigreeRating > Result then Result := Puppy.PedigreeRating;
end;

class procedure TAnimalUtils.Feed<ElemType>(List: TAnimalList<ElemType>);
begin
  Writeln('Feeding the animals...');
  List.AllMakeSound;
  Writeln('Urgh, maybe later');
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
    TAnimalUtils.Feed<TPuppy>(Puppies);
    WriteLn;
    //create the animal list, add a piranha, a snake, the puppies, and another snake
    Animals := TAnimalList.Create;
    Animals.Add(TPiranha.Create);
    Animals.Add(TSnake.Create);
    Puppies.MoveTo<TAnimal>(Animals);
    Animals.Add(TSnake.Create);
    //feed the animals again
    TAnimalUtils.Feed<TAnimal>(Animals);
  finally
    Puppies.Free;
    Animals.Free;
  end;
  ReadLn;
end.

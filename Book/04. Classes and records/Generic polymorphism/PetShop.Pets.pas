unit PetShop.Pets;

interface

type
  TAnimal = class abstract
    procedure MakeSound; virtual; abstract;
  end;

  TPiranha = class(TAnimal)
    procedure MakeSound; override;
  end;

  TSnake = class(TAnimal)
    procedure MakeSound; override;
  end;

  TPedigreeRating = (prLow, prMedium, prHigh);

  TPuppy = class(TAnimal)
  strict private
    FPedigreeRating: TPedigreeRating;
  public
    constructor Create(APedigreeRating: TPedigreeRating = prMedium);
    procedure MakeSound; override;
    property PedigreeRating: TPedigreeRating read FPedigreeRating write FPedigreeRating;
  end;

const
  PedigreeRatingStrings: array[TPedigreeRating] of string = (
    'mongrel', 'typical', 'pure bred');

implementation

procedure TPiranha.MakeSound;
begin
  WriteLn('Swoosh!');
end;

procedure TSnake.MakeSound;
begin
  WriteLn('Hisssss...');
end;

constructor TPuppy.Create(APedigreeRating: TPedigreeRating = prMedium);
begin
  inherited Create;
  FPedigreeRating := APedigreeRating;
end;

procedure TPuppy.MakeSound;
begin
  WriteLn('Woof!');
end;

end.

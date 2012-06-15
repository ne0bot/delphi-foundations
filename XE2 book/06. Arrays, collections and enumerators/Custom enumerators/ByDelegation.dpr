program ByDelegation;
{
  Demonstrates implementing a custom enumerator by exposing the enumerator of an
  encapsulated object. While the FSubObjects field could be exposed directly, that
  would mean consumer code could call its Add and Delete methods as they please. Keeping
  it encapsulated also enables TParentObject to change the class type over time. E.g.,
  the TObjectList could be replaced with a TObjectDictionary, TParentObject switching to
  expose the latter's Values enumerator without changing its own interface.
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Generics.Collections;

type
  TParentObject = class;

  TSubObject = class
  strict private
    FData: string;
    FOwner: TParentObject;
  public
    constructor Create(AOwner: TParentObject; const AData: string);
    property Data: string read FData;
    property Owner: TParentObject read FOwner;
  end;

  TParentObject = class
  strict private
    FSubObjects: TObjectList<TSubObject>;
  public
    constructor Create;
    destructor Destroy; override;
    function GetEnumerator: TEnumerator<TSubObject>;
  end;

constructor TSubObject.Create(AOwner: TParentObject; const AData: string);
begin
  inherited Create;
  FOwner := AOwner;
  FData := AData;
end;

constructor TParentObject.Create;
begin
  inherited Create;
  FSubObjects := TObjectList<TSubObject>.Create;
  FSubObjects.Add(TSubObject.Create(Self, 'First'));
  FSubObjects.Add(TSubObject.Create(Self, 'Second'));
  FSubObjects.Add(TSubObject.Create(Self, 'Third'));
end;

destructor TParentObject.Destroy;
begin
  FSubObjects.Free;
  inherited Destroy;
end;

function TParentObject.GetEnumerator: TEnumerator<TSubObject>;
begin
  Result := FSubObjects.GetEnumerator;
end;

var
  Parent: TParentObject;
  SubObj: TSubObject;
begin
  Parent := TParentObject.Create;
  try
    for SubObj in Parent do
      WriteLn(SubObj.Data);
  finally
    Parent.Free;
  end;
  ReadLn;
end.

program _CustomEqualityComparers;
{
  Equality comparers are used with dictionary objects to customise how key matching
  works. The code below demonstrates (i) a custom string comparer which works case
  insensitively and (ii) overriding the Equals and GetHashCode methods of a class to
  more appropriately handle matters when the class is used as the key type of a
  dictionary. NB - the demo ends with a deliberate exception that shows the custom
  comparer previous set actually made a difference!
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.Classes, System.Generics.Defaults, System.Generics.Collections;

procedure TestStringDictionary(const AComparer: IEqualityComparer<string>);
var
  Dict: TDictionary<string, string>;
begin
  WriteLn('Testing...');
  Dict := TDictionary<string, string>.Create(AComparer);
  try
    Dict.Add('', 'Dummy');
    Dict.Add('McLaren', 'Vodafone McLaren Mercedes');
    Dict.Add('Renault', 'Renault GP');
    Dict.Add('Williams', 'AT&T Williams');
    WriteLn('... changing value case insensitively');
    Dict['RENAULT'] := 'Lotus Renault GP';
  finally
    Dict.Free;
  end;
  WriteLn('Success');
  WriteLn;
end;

type
  TCaseInsensitiveComparer = class(TInterfacedObject, IEqualityComparer<string>)
  protected
    function Equals(const Left, Right: string): Boolean; reintroduce;
    function GetHashCode(const Value: string): Integer; reintroduce;
  end;

function TCaseInsensitiveComparer.Equals(const Left, Right: string): Boolean;
begin
  Result := SameText(Left, Right, loUserLocale);
end;

function TCaseInsensitiveComparer.GetHashCode(const Value: string): Integer;
var
  S: string;
begin
  if S = '' then Exit(42); //any dummy number will do
  S := UpperCase(Value, loUserLocale);
  Result := BobJenkinsHash(S[1], Length(S) * StringElementSize(S), 0);
end;

type
  TTestKeyObject = class
  strict private
    FData: Integer;
  public
    constructor Create(AData: Integer);
    property Data: Integer read FData;
  end;

  TTestKeyObjectClass = class of TTestKeyObject;

  TTestKeyObjectWithEqualsOverride = class(TTestKeyObject)
  public
    function Equals(Obj: TObject): Boolean; override;
    function GetHashCode: Integer; override;
  end;

constructor TTestKeyObject.Create(AData: Integer);
begin
  inherited Create;
  FData := AData;
end;

function TTestKeyObjectWithEqualsOverride.Equals(Obj: TObject): Boolean;
begin
  if Obj = Self then Exit(True);
  if not (Obj is TTestKeyObject) then Exit(False);
  Result := TTestKeyObject(Obj).Data = Data;
end;

function TTestKeyObjectWithEqualsOverride.GetHashCode: Integer;
begin
  Result := Data;
end;

procedure TestKeyObject(AClass: TTestKeyObjectClass);
var
  Dict: TDictionary<TTestKeyObject,string>;
  Obj1, Obj2: TTestKeyObject;
begin
  WriteLn('*** ', AClass.ClassName, ' ***');
  Obj1 := nil; Obj2 := nil;
  Dict := TDictionary<TTestKeyObject,string>.Create;
  try
    Obj1 := AClass.Create(999);
    WriteLn('Adding object with ', Obj1.Data, ' as its data...');
    Dict.Add(Obj1, 'Emergency!');
    Obj2 := AClass.Create(999);
    WriteLn('Testing second object with ', Obj2.Data, ' as its data...');
    if Dict.ContainsKey(Obj2) then
      WriteLn('Already have that key')
    else
      WriteLn('DON''T already have that key');
  finally
    Dict.Free;
    Obj1.Free;
    Obj2.Free;
  end;
  WriteLn;
end;

begin
  try
    TestKeyObject(TTestKeyObject);
    TestKeyObject(TTestKeyObjectWithEqualsOverride);
    { using an explicit helper class }
    TestStringDictionary(TCaseInsensitiveComparer.Create);
    { using a pair of anonymous methods }
    TestStringDictionary(TEqualityComparer<string>.Construct(
      function (const L, R: string): Boolean
      begin
        Result := SameText(L, R, loUserLocale);
      end,

      function (const Value: string): Integer
      var
        S: string;
      begin
        if S = '' then Exit(42); //any dummy number will do
        S := UpperCase(Value, loUserLocale);
        Result := BobJenkinsHash(S[1], Length(S) * StringElementSize(S), 0);
      end));
    { using the default comparer (will cause an exception, given the default
      comparer is case sensitive) }
    TestStringDictionary(nil);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.

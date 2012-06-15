program CustomEqualityComparers;
{
  Demonstrates custom equality comparers, the first by customising the Equals
  and GetHashCode methods of a class that could potentially be used as a
  dictionary key type, and the second by providing an explicit IEqualityComparer
  implementation for a record type that compares string field values case
  insensitively.

  Writing a case insensitive equality comparer has its subtleties if you want to
  work with Unicode characters properly. The obvious way to implement Equals is
  to call CompareText(S1, S1, loUserLocale), however this may lead to
  discrepencies with GetHashCode due to CompareText silently putting together
  decomposed characters on the fly. What TNameRecEqualityComparer does here
  broadly follows what TIStringComparer in System.Generic.Defaults does, which
  privileges consistency between Equals and GetHashCode over strict correctness
  from a human-readable POV for Equals.
}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults;

type
  TInt64Object = class
    Data: Int64;
    function Equals(Obj: TObject): Boolean; override;
    function GetHashCode: Integer; override;
  end;

function TInt64Object.Equals(Obj: TObject): Boolean;
begin
  if Obj = Self then
    Result := True
  else if not (Obj is TInt64Object) then
    Result := False
  else
    Result := TInt64Object(Obj).Data = Data;
end;

function TInt64Object.GetHashCode: Integer;
begin
  Result := BobJenkinsHash(Data, SizeOf(Data), 0);
end;

type
  TNameRec = record
    Forename, Surname: string;
    constructor Create(const AForename, ASurname: string);
  end;

  { NB: the 'reintroduce' directive prevents the compiler from warning us that we are hiding
    similarly named virtual methods defined by TObject. }
  TNameRecEqualityComparer = class(TInterfacedObject, IEqualityComparer<TNameRec>)
    function Equals(const Left, Right: TNameRec): Boolean; reintroduce;
    function GetHashCode(const Value: TNameRec): Integer; reintroduce;
  end;

constructor TNameRec.Create(const AForename, ASurname: string);
begin
  Forename := AForename;
  Surname := ASurname;
end;

function TNameRecEqualityComparer.Equals(const Left, Right: TNameRec): Boolean;
var
  S1, S2: string;
begin
  S1 := LowerCase(Left.Forename + Left.Surname, loUserLocale);
  S2 := LowerCase(Right.Forename + Right.Surname, loUserLocale);
  Result := (Length(S1) = Length(S2)) and
    CompareMem(PChar(S1), PChar(S2), ByteLength(S1));
end;

function TNameRecEqualityComparer.GetHashCode(const Value: TNameRec): Integer;
var
  S: string;
begin
  S := LowerCase(Value.Forename + Value.Surname, loUserLocale);
  Result := BobJenkinsHash(PChar(S)^, ByteLength(S), 0);
end;

var
  NameToAgeMap: TDictionary<TNameRec,Integer>;
  Name: TNameRec;
begin
  NameToAgeMap := TDictionary<TNameRec,Integer>.Create(
    TNameRecEqualityComparer.Create);
  try
    //add some items
    NameToAgeMap.Add(TNameRec.Create('John', 'Smith'), 48);
    NameToAgeMap.Add(TNameRec.Create('Joe', 'Bloggs'), 59);
    //test whether there's a Joe Smith in there
    Name := TNameRec.Create('JOE', 'SMITH');
    WriteLn(NameToAgeMap.ContainsKey(Name)); //output: FALSE
    //test whether there's a Joe Bloggs in there
    Name.Surname := 'BLOGGS';
    WriteLn(NameToAgeMap.ContainsKey(Name)); //output: TRUE
  finally
    NameToAgeMap.Free;
  end;
  ReadLn;
end.

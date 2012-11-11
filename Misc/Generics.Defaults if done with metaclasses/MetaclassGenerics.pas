unit MetaclassGenerics;
{
  (Incompete) demo of the metaclass alternative to interfaces in the case of
  Generics.Defaults. An ultimate base class that is non-generic is partly a workaround
  for the fact 'class of' declarations don't work with generic classes, and partly
  because untyped comparisons cuts down the code for the various possible ordinal types.
  See http://delphihaven.wordpress.com/2011/03/24/implementing-generic-helpers-as-metaclasses/
  for more discussion.
}
interface

uses
  SysUtils;

type
  TComparer = class;

  TComparerClass = class of TComparer;

  TComparer = class abstract
    class function CompareUntyped(const Left, Right): Integer; virtual; abstract;
    class function Default<T>: TComparerClass; static;
  end;

  TTypedComparer<T> = class abstract(TComparer) //descendants must overide at least one of CompareUntyped and Compare
    class function Compare(const Left, Right: T): Integer; overload; virtual;
    class function CompareUntyped(const Left, Right): Integer; overload; override;
  end;

  TBinaryComparer<T> = class(TTypedComparer<T>)
    class function CompareUntyped(const Left, Right): Integer; override;
  end;

  TUInt8Comparer = class(TTypedComparer<UInt8>) //Byte, AnsiChar
    class function CompareUntyped(const Left, Right): Integer; override;
  end;

  TUInt16Comparer = class(TTypedComparer<UInt16>) //Word, WideChar
    class function CompareUntyped(const Left, Right): Integer; override;
  end;

  TInt32Comparer = class(TTypedComparer<Int32>)
    class function CompareUntyped(const Left, Right): Integer; override;
  end;

  TUInt32Comparer = class(TTypedComparer<UInt32>) //also for 32 bit pointers and reference types
    class function CompareUntyped(const Left, Right): Integer; override;
  end;

  TInt64Comparer = class(TTypedComparer<Int64>)
    class function CompareUntyped(const Left, Right): Integer; override;
  end;

  TUInt64Comparer = class(TTypedComparer<UInt64>)
    class function CompareUntyped(const Left, Right): Integer; override;
  end;

  TUnicodeStringComparer = class(TTypedComparer<UnicodeString>)
    class function Compare(const Left, Right: UnicodeString): Integer; override;
  end;

  TStringComparer = TUnicodeStringComparer;

  TList<T> = class
  private
    FItems: array of T;
    FCount: Integer;
    FComparer: TComparerClass;
    function GetItem(Index: Integer): T;
  public
    constructor Create(const AComparer: TComparerClass = nil); overload;
    function Add(const Value: T): Integer;
    function IndexOf(const Value: T): Integer;
    property Count: Integer read FCount;
    property Items[Index: Integer]: T read GetItem; default;
  end;

implementation

uses RTLConsts, TypInfo, Generics.Defaults{for BinaryCompare};

{ TComparer }

class function TComparer.Default<T>: TComparerClass;
var
  Info: PTypeInfo;
begin
  Info := TypeInfo(T);
  case Info.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
      case GetTypeData(Info).OrdType of
        otUByte: Exit(TUInt8Comparer);
        otUWord: Exit(TUInt16Comparer);
        otSLong: Exit(TInt32Comparer);
        otULong: Exit(TUInt32Comparer);
      end;
    tkClass, tkInterface, tkPointer, tkClassRef, tkProcedure, tkDynArray:
      Exit({$IF SizeOf(Pointer) = 4}TUInt32Comparer{$ELSE}TUInt64Comparer{$IFEND});
    tkInt64: Exit(TInt64Comparer);
    tkUString: Exit(TUnicodeStringComparer);
  end;
  Result := TBinaryComparer<T>;
end;

{ TTypedComparer<T> }

class function TTypedComparer<T>.CompareUntyped(const Left, Right): Integer;
begin
  Result := Compare(T(Left), T(Right));
end;

class function TTypedComparer<T>.Compare(const Left, Right: T): Integer;
begin
  Result := CompareUntyped(Left, Right);
end;

{ TXXXComparer }

class function TBinaryComparer<T>.CompareUntyped(const Left, Right): Integer;
begin
  Result := BinaryCompare(@Left, @Right, SizeOf(T));
end;

class function TUInt8Comparer.CompareUntyped(const Left, Right): Integer;
begin
  if UInt8(Left) < UInt8(Right) then
    Result := -1
  else if UInt8(Left) > UInt8(Right) then
    Result := 1
  else
    Result := 0;
end;

class function TUInt16Comparer.CompareUntyped(const Left, Right): Integer;
begin
  if UInt16(Left) < UInt16(Right) then
    Result := -1
  else if UInt16(Left) > UInt16(Right) then
    Result := 1
  else
    Result := 0;
end;

class function TInt32Comparer.CompareUntyped(const Left, Right): Integer;
begin
  if Int32(Left) < Int32(Right) then
    Result := -1
  else if Int32(Left) > Int32(Right) then
    Result := 1
  else
    Result := 0;
end;

class function TUInt32Comparer.CompareUntyped(const Left, Right): Integer;
begin
  if UInt32(Left) < UInt32(Right) then
    Result := -1
  else if UInt32(Left) > UInt32(Right) then
    Result := 1
  else
    Result := 0;
end;

class function TInt64Comparer.CompareUntyped(const Left, Right): Integer;
begin
  if Int64(Left) < Int64(Right) then
    Result := -1
  else if Int64(Left) > Int64(Right) then
    Result := 1
  else
    Result := 0;
end;

class function TUInt64Comparer.CompareUntyped(const Left, Right): Integer;
begin
  if UInt64(Left) < UInt64(Right) then
    Result := -1
  else if UInt64(Left) > UInt64(Right) then
    Result := 1
  else
    Result := 0;
end;

class function TUnicodeStringComparer.Compare(const Left, Right: UnicodeString): Integer;
begin
  Result := AnsiCompareStr(Left, Right);
end;

{ TList<T> }

constructor TList<T>.Create(const AComparer: TComparerClass);
begin
  inherited Create;
  if AComparer <> nil then
    FComparer := AComparer
  else
    FComparer := TComparer.Default<T>;
end;

function TList<T>.Add(const Value: T): Integer;
begin
  if Length(FItems) = FCount then
    SetLength(FItems, FCount + 8); //and yeah, that is a dumb allocation strategy...
  FItems[FCount] := Value;
  Result := FCount;
  Inc(FCount);
end;

function TList<T>.GetItem(Index: Integer): T;
begin
  if (Index < 0) or (Index >= FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  Result := FItems[Index];
end;

function TList<T>.IndexOf(const Value: T): Integer;
begin
  for Result := 0 to Count - 1 do
    if FComparer.CompareUntyped(FItems[Result], Value) = 0 then Exit;
  Result := -1;
end;

end.

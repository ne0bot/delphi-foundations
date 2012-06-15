unit CCR.Generics.SimpleArraySearch;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Generics.Defaults;

type
  TArrayHelper = class helper for TArray
    class function FindFirst<T>(const Values: array of T; const Item: T;
      out FoundIndex: Integer): Boolean; overload;
    class function FindFirst<T>(const Values: array of T; const Item: T;
      out FoundIndex: Integer; const Comparer: IComparer<T>): Boolean; overload;
    class function FindFirst<T>(const Values: array of T; const Item: T;
      out FoundIndex: Integer; const Comparer: IComparer<T>;
      Index, Count: Integer): Boolean; overload;
  end;

implementation

uses System.RTLConsts;

class function TArrayHelper.FindFirst<T>(const Values: array of T; const Item: T;
  out FoundIndex: Integer): Boolean;
begin
  Result := FindFirst<T>(Values, Item, FoundIndex, TComparer<T>.Default,
    0, Length(Values))
end;

class function TArrayHelper.FindFirst<T>(const Values: array of T; const Item: T;
  out FoundIndex: Integer; const Comparer: IComparer<T>): Boolean;
begin
  Result := FindFirst<T>(Values, Item, FoundIndex, Comparer, 0, Length(Values))
end;

class function TArrayHelper.FindFirst<T>(const Values: array of T; const Item: T;
  out FoundIndex: Integer; const Comparer: IComparer<T>;
  Index, Count: Integer): Boolean;
var
  I: Integer;
begin
  if (Index < Low(Values)) or (Index + Count > Length(Values)) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  for I := Index to Count - 1 do
    if Comparer.Compare(Values[I], Item) = 0 then
    begin
      FoundIndex := I;
      Exit(True);
    end;
  Exit(False);
end;

end.

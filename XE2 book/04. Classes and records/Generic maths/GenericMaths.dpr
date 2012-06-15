program GenericMaths;
{
  The range of possible generic constraints in XE2 is rather small, since it doesn't include
  things like operator constraints. One workaround for that particular lack is to define helper
  types to plug in; this example we use metaclasses. These have the benefit of not requiring
  explicit instantiation.
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.Generics.Collections;

type
  { Helper type base class; for the demo, we'll just define a substitute
    method for the + operator. If done for real, you would probably have
    Substract, Multiply and Divide methods on the same pattern.	}
  TCalculator<T> = class abstract
    class function Add(const A, B: T): T; virtual; abstract;
  end;

  { Helper type  for Double }
  TDoubleCalculator = class(TCalculator<Double>)
    class function Add(const A, B: Double): Double; override;
  end;

  { Helper type  for Integer }
  TIntegerCalculator = class(TCalculator<Integer>)
    class function Add(const A, B: Integer): Integer; override;
  end;

  { Helper type  for Single }
  TSingleCalculator = class(TCalculator<Single>)
    class function Add(const A, B: Single): Single; override;
  end;

  { Generic that makes use of a helper type }
  TGenericMaths<T; Calculator: TCalculator<T>> = record
    class function Sum(const ANumbers: array of T): T; static;
  end;

class function TDoubleCalculator.Add(const A, B: Double): Double;
begin
  Result := A + B;
end;

class function TIntegerCalculator.Add(const A, B: Integer): Integer;
begin
  Result := A + B;
end;

class function TSingleCalculator.Add(const A, B: Single): Single;
begin
  Result := A + B;
end;

class function TGenericMaths<T, Calculator>.Sum(const ANumbers: array of T): T;
var
  Elem: T;
begin
  Result := Default(T);
  for Elem in ANumbers do
    Result := Calculator.Add(Result, Elem);
end;

var
  IntegerArray: array[1..100000] of Integer;
  SingleArray: array[1..100000] of Single;
  DoubleArray: array[1..100000] of Double;
  I, IntegerTotal: Integer;
  SingleTotal: Single;
  DoubleTotal: Single;
begin
  Randomize;
  for I := Low(IntegerArray) to High(IntegerArray) do
    IntegerArray[I] := Random(20);
  IntegerTotal := TGenericMaths<Integer,TIntegerCalculator>.Sum(IntegerArray);
  WriteLn('The integer total is ', IntegerTotal);
  for I := Low(DoubleArray) to High(DoubleArray) do
    DoubleArray[I] := Random;
  DoubleTotal := TGenericMaths<Double,TDoubleCalculator>.Sum(DoubleArray);
  WriteLn('The double total is ', IntegerTotal);
  for I := Low(SingleArray) to High(SingleArray) do
    SingleArray[I] := Random;
  SingleTotal := TGenericMaths<Single,TSingleCalculator>.Sum(SingleArray);
  WriteLn('The single total is ', IntegerTotal);
  ReadLn;
end.

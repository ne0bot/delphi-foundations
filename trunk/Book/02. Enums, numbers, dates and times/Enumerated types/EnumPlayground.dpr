program EnumPlayground;
{
  This is the example code from the 'Enumerated types' section of chapter 2. Be warned that the
  attempt to use TEnumConvertor with TFunkyEnum will cause an exception to be raised. This is
  intentional, and demonstrates the fact a generic approach cannot be taken with non-continuous
  enumerations. Just press Continue if running under the debugger, since the exception will be
  caught by the program.
}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.TypInfo;

type
  TEnumConvertor<T: record> = record
    class function TryFromInteger(OrdValue: Integer;
      var EnumValue: T): Boolean; static;
    class function TryFromString(const Str: string;
      var EnumValue: T): Boolean; static;
    class function ToString(Value: T): string; static;
  end;

  TDayOfWeek = (Mon, Tues, Weds, Thurs, Fri, Sat, Sun);

{$SCOPEDENUMS ON}
  TStarRating = (None, One, Two, Three, Four, Five);
  TResponsibilityLevel = (None, Supervisor, Organiser, Executive);
{$SCOPEDENUMS OFF}

  TFunkyEnum = (OutOfSight = 64, IGotYou, GetUpIFeel = 70);

class function TEnumConvertor<T>.TryFromInteger(OrdValue: Integer;
  var EnumValue: T): Boolean;
var
  Info: PTypeInfo;
  Data: PTypeData;
begin
  Info := TypeInfo(T);
  //check we have a valid type!
  if (Info = nil) or (Info.Kind <> tkEnumeration) then
    raise EArgumentException.Create('TEnumConvertor not instantiated with a valid type');
  //do the actual range test
  Data := GetTypeData(Info);
  Result := (OrdValue >= Data.MinValue) and (OrdValue <= Data.MaxValue);
  if Result then Move(OrdValue, EnumValue, SizeOf(T));
end;

class function TEnumConvertor<T>.TryFromString(const Str: string;
  var EnumValue: T): Boolean;
var
  Info: PTypeInfo;
  IntValue: Integer;
begin
  IntValue := GetEnumValue(TypeInfo(T), Str);
  Result := (IntValue <> -1);
  if Result then Move(IntValue, EnumValue, SizeOf(T));
end;

class function TEnumConvertor<T>.ToString(Value: T): string;
var
  IntValue: Integer;
begin
  IntValue := 0;
  Move(Value, IntValue, SizeOf(T));
  Result := GetEnumName(TypeInfo(T), IntValue)
end;

function TryIntToFunkyEnum(OrdValue: Integer;
  var Track: TFunkyEnum): Boolean;
var
  Value: TFunkyEnum;
begin
  Value := TFunkyEnum(OrdValue);
  Result := Value in [OutOfSight, IGotYou, GetUpIFeel];
  if Result then Track := Value;
end;

var
  Day: TDayOfWeek;
  Rating: TStarRating;
  FunkyValue: TFunkyEnum;
begin
  Day := Fri;
  if Day < Tues then
    WriteLn('Fri is before Tues... huh?')
  else if Day > Weds then
    WriteLn('Fri is after Weds');
  WriteLn;

  if TEnumConvertor<TDayOfWeek>.TryFromInteger(1, Day) then
    WriteLn('1 is valid for TDayOfWeek')
  else
    WriteLn('1 in invalid for TDayOfWeek');
  WriteLn;

  if TEnumConvertor<TStarRating>.TryFromInteger(99, Rating) then
    WriteLn('99 is valid for TStarRating')
  else
    WriteLn('99 is invalid for TStarRating');
  WriteLn;

  WriteLn('Using the generic helper type, can 68 be converted into a TFunkyEvum value?');
  try
    TEnumConvertor<TFunkyEnum>.TryFromInteger(68, FunkyValue);
  except
    on E: Exception do WriteLn(E.ClassName, ': ', E.Message);
  end;
  WriteLn;
  WriteLn('Can 68 *really* be converted into a valid TFunkyEvum value? ',
    TryIntToFunkyEnum(68, FunkyValue));
  WriteLn;

  WriteLn('Is the string ''rubbish'' a valid TDayOfWeek value? ',
    TEnumConvertor<TDayOfWeek>.TryFromString('rubbish', Day));
  WriteLn;
  WriteLn('Is the string ''MON'' a valid TDayOfWeek value? ',
    TEnumConvertor<TDayOfWeek>.TryFromString('MON', Day));
  WriteLn;

  WriteLn('The first day of the week is ',
    TEnumConvertor<TDayOfWeek>.ToString(Low(TDayOfWeek)));

  ReadLn;
end.

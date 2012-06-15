program TypInfoBasics;
{
  Simple demo of retrieving (very) basic type information using the TypInfo unit.
}
{$APPTYPE CONSOLE}

uses
  Classes,
  TypInfo;

const
  OrdinalTypeKinds = [tkInteger, tkChar, tkEnumeration, tkWChar];

procedure OutputOrdinalInfo(const TypeInfo: PTypeInfo);
var
  Data: PTypeData;
begin
  WriteLn(TypeInfo.Name);
  if not (TypeInfo.Kind in OrdinalTypeKinds) then
  begin
    WriteLn('Not an ordinal type!');
    Exit;
  end;
  Data := GetTypeData(TypeInfo);
  WriteLn('Min ordinal value = ', Data.MinValue);
  WriteLn('Max ordinal value = ', Data.MaxValue);
  Write('Stored as a ');
  case Data.OrdType of
    otSByte: WriteLn('signed byte value');
    otUByte: WriteLn('unsigned byte value');
    otSWord: WriteLn('signed 2-byte value');
    otUWord: WriteLn('unsigned 2-byte value');
    otSLong: WriteLn('signed 4-byte value');
    otULong: WriteLn('unsigned 4-byte value');
  end;
  WriteLn;
end;

procedure OutputInfo(const TypeInfo: PTypeInfo);
var
  Data: PTypeData;
begin
  if TypeInfo.Kind in OrdinalTypeKinds then
  begin
    OutputOrdinalInfo(TypeInfo);
    Exit;
  end;
  Data := GetTypeData(TypeInfo);
  WriteLn(TypeInfo.Name);
  case TypeInfo.Kind of
    tkLString: WriteLn('Code page: ', Data.CodePage);
    tkFloat:
    begin
      Write('Underlying type: ');
      case Data.FloatType of
        ftSingle: WriteLn('Single');
        ftDouble: WriteLn('Double');
        ftExtended: WriteLn('Extended');
        ftCurr: WriteLn('Currency');
      end;
    end;
    tkClass:
    begin
      WriteLn('Total no. of published props: ', Data.PropCount);
      WriteLn('Unit name: ', Data.UnitName);
    end;
  end;
  WriteLn;
end;

type
  GreekAnsiString = type AnsiString(1253);
  TMyEnum = (First, Second, Third);
  TMySubRange = 10..99;

begin
  OutputInfo(TypeInfo(UTF8String));
  OutputInfo(TypeInfo(GreekAnsiString));
  OutputInfo(TypeInfo(Real));
  OutputInfo(TypeInfo(TComponent));
  OutputOrdinalInfo(TypeInfo(Boolean));
  OutputOrdinalInfo(TypeInfo(Char));
  OutputOrdinalInfo(TypeInfo(Integer));
  OutputOrdinalInfo(TypeInfo(TMyEnum));
  OutputOrdinalInfo(TypeInfo(TMySubRange));
  Readln;
end.

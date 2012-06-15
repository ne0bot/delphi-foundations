program StrToFloatConvs;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Math;

const
  TestAngloStr = '123.56';
  TestFrenchStr = '123,56';
var
  FrenchSettings, BritishSettings: TFormatSettings;
  Value: Double;
begin
  FrenchSettings := TFormatSettings.Create('fr-FR');
  BritishSettings := TFormatSettings.Create('en-GB');
  WriteLn('Anglophone-style test string (', TestAngloStr, ')');
  WriteLn('Naive TryStrToFloat: ', TryStrToFloat(TestAngloStr, Value));
  WriteLn('TryStrToFloat with fr-FR: ', TryStrToFloat(TestAngloStr, Value, FrenchSettings));
  WriteLn('TryStrToFloat with en-GB: ', TryStrToFloat(TestAngloStr, Value, BritishSettings));
  WriteLn;
  WriteLn('Francophone-style test string (', TestFrenchStr, ')');
  WriteLn('Naive TryStrToFloat: ', TryStrToFloat(TestFrenchStr, Value));
  WriteLn('TryStrToFloat with fr-FR: ', TryStrToFloat(TestFrenchStr, Value, FrenchSettings));
  WriteLn('TryStrToFloat with en-GB: ', TryStrToFloat(TestFrenchStr, Value, BritishSettings));
  ReadLn;
end.

program CurrencyVsDouble;
{
  Simple demo of the pros and cons of the Currency and Double types.
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils;

var
  I: Integer;
  CurrValue: Currency;
  DblValue: Double;
begin
  CurrValue := 0;
  DblValue := 0;
  for I := 1 to 90 do
  begin
    CurrValue := CurrValue + 0.1;
    DblValue := DblValue + 0.1;
  end;
  WriteLn(CurrToStr(CurrValue)); //outputs 9 (correct!)
  WriteLn(FloatToStr(DblValue)); //outputs 8.99999999999998 (wrong!)
  WriteLn;
  CurrValue := 1;
  DblValue := 1;
  for I := 1 to 5 do
  begin
    CurrValue := CurrValue * 0.1;
    DblValue := DblValue * 0.1;
  end;
  WriteLn(CurrToStr(CurrValue)); //outputs 0 (been rounded to 4 decimal places)
  WriteLn(FloatToStr(DblValue)); //outputs 1E-5, i.e. 0.0000000001 (correct!)
  ReadLn;
end.

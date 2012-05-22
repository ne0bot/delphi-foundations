program MeasurementConversions;
{
  Example of adding custom conversion unit types to the RTL's standard set.
}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.ConvUtils,
  System.StdConvs,
  CustomConvs in 'CustomConvs.pas';

var
  Miles: Double;
  Fahrenheit, Celsius: Double;
  I: Integer;
begin
  //convert two Roman miles to two statute (i.e., modern) miles
  Miles := Convert(2, duRomanMile, duMiles);
  WriteLn(Format('2 Roman miles = %g statute miles', [Miles]));
  //convert gas mark ¼
  Fahrenheit := Convert(0.25, tuGasMark, tuFahrenheit);
  Celsius := Convert(0.25, tuGasMark, tuCelsius);
  WriteLn(Format('Gas mark 1/4 is %g°F or %.0f°C', [Fahrenheit, Celsius]));
  //convert gas mark ½
  Fahrenheit := Convert(0.5, tuGasMark, tuFahrenheit);
  Celsius := Convert(0.5, tuGasMark, tuCelsius);
  WriteLn(Format('Gas mark 1/2 is %g°F or %.0f°C', [Fahrenheit, Celsius]));
  //convert gas marks 1-9
  for I := 1 to 9 do
  begin
    Fahrenheit := Convert(I, tuGasMark, tuFahrenheit);
    Celsius := Convert(I, tuGasMark, tuCelsius);
    WriteLn(Format('Gas mark %d is %g°F or %.0f°C', [I, Fahrenheit, Celsius]));
  end;
  ReadLn;
end.

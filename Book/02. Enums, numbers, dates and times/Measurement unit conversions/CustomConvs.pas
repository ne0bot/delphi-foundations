unit CustomConvs;
{
  Example of registering custom conversion types, here the Roman mile and the Gas Mark temperature
  scale. Since converting between Roman miles and the distance family base type (metres) involves
  a simple ratio only, we don't need to define explicit conversion functions. These however are
  required in the Gas Mark case.
}
interface

uses
  System.Math, System.ConvUtils, System.StdConvs;

var
  duRomanMile, tuGasMark: TConvType;

function GasMarkToCelsius(const AValue: Double): Double;
function CelsiusToGasMark(const AValue: Double): Double;

implementation

function GasMarkToCelsius(const AValue: Double): Double;
begin
  if AValue >= 1 then
    Result := (243 + 25 * (AValue - 1)) * 5 / 9
  else
    Result := (168 + 100 * AValue) * 5 / 9;
end;

function CelsiusToGasMark(const AValue: Double): Double;
begin
  if AValue >= 135 then
    Result := ((AValue * 9 / 5 - 243) / 25) + 1
  else
    Result := ((AValue * 9 / 5) - 168) / 100;
end;

initialization
  duRomanMile := RegisterConversionType(cbDistance, 'Roman mile', 1479);
  tuGasMark := RegisterConversionType(cbTemperature, 'Gas mark', GasMarkToCelsius, CelsiusToGasMark);
finalization
  UnregisterConversionType(duRomanMile);
  UnregisterConversionType(tuGasMark);
end.

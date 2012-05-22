program DoubleInternals;

{$APPTYPE CONSOLE}

{$R *.res}

procedure OutputDoubleDetails(const Value: Double);
var
  I: Integer;
  Rec: TDoubleRec;
begin
  WriteLn(Value);
  Rec := TDoubleRec(Value);
  Write('Type: ');
  case Rec.SpecialType of
    fsZero: Writeln('zero');
    fsNZero: Writeln('negative zero');
    fsDenormal: Writeln('denormal');
    fsNDenormal: Writeln('negative denormal');
    fsPositive: Writeln('positive');
    fsNegative: Writeln('negative');
    fsInf: Writeln('infinity');
    fsNInf: Writeln('negative infinity');
    fsNaN: Writeln('not a number (NaN)');
  end;
  WriteLn('Exponent: ', Rec.Exponent);
  WriteLn('Exponent (raw): ', Rec.Exp);
  WriteLn('Fraction: ', Rec.Fraction);
  WriteLn('Fraction (raw): ', Rec.Frac);
  Write('Sign: ');
  if Rec.Sign then
    WriteLn('-')
  else
    WriteLn('+');
  Write('Raw bytes: ', Rec.Bytes[0]);
  for I := 1 to High(Rec.Bytes) do
    Write(', ', Rec.Bytes[I]);
  WriteLn(sLineBreak);
end;

var
  DblValue: Double;
begin
  DblValue := -5.5;
  OutputDoubleDetails(DblValue);

  DblValue := 123.589;
  OutputDoubleDetails(DblValue);

  DblValue := 0/0;
  OutputDoubleDetails(DblValue);

  Write('Press ENTER to exit...');
  ReadLn;
end.

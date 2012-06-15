program SetOperators;
{
  Demonstrates the various set operators.
}
{$APPTYPE CONSOLE}

type
  TQuality = (qlClever, qlQuick, qlResourceful, qlThorough);
  TQualities = set of TQuality;

procedure OutputQualities(Qualities: TQualities); overload;
const
  SQualities: array[TQuality] of string = ('Clever', 'Quick', 'Resourceful', 'Thorough');
var
  Quality: TQuality;
begin
  for Quality in Qualities do
    WriteLn('- ', SQualities[Quality]);
  WriteLn;
end;

procedure OutputQualities(const Name: string; Qualities: TQualities); overload;
begin
  WriteLn('Qualities of ', Name, ':');
  OutputQualities(Qualities);
end;

var
  QualitiesOfSam, QualitiesOfBob: TQualities;
begin
  QualitiesOfSam := [qlClever, qlResourceful, qlThorough];
  QualitiesOfBob := [qlResourceful, qlThorough];

  OutputQualities('Sam', QualitiesOfSam);
  OutputQualities('Bob', QualitiesOfBob);

  WriteLn('Out of being clever and resourceful, Bob is:');
  OutputQualities(QualitiesOfBob * [qlClever, qlResourceful]);

  Write('Does Sam have exactly the same qualities as Bob? ');
  WriteLn(QualitiesOfSam = QualitiesOfBob);                 //false
  if QualitiesOfSam <= QualitiesOfBob then                  //no
    WriteLn('Sam has no quality beyond what Bob has.');
  if QualitiesOfSam >= QualitiesOfBob then                  //yes
  begin
    WriteLn('Sam has at least the qualities Bob has.');
    if QualitiesOfSam <> QualitiesOfBob then                //yes
      WriteLn('In fact, Sam has more qualities than Bob.');
  end;

  WriteLn;
  WriteLn('Their shared qualities are the following:');
  OutputQualities(QualitiesOfSam * QualitiesOfBob);
  WriteLn('The extra qualities Sam has are these:');
  OutputQualities(QualitiesOfSam - QualitiesOfBob);
  Readln;
end.

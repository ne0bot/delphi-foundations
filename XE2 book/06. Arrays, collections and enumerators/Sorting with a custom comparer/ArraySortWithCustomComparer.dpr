program ArraySortWithCustomComparer;
{
  Demonstrates using a custom comparer with the TArray helper class for dynamic
  arrays - note that the same comparer could be employed were a TList be used
  instead of an array.

  When defining an array or list of records, a custom comparer is almost always
  necessary since the default one for records just does a binary comparison. This
  is fast, but takes no account of the record's subdivision into distinct fields!
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.Generics.Collections, System.Generics.Defaults;

type
  TPersonRec = record
    Forename, Surname: string;
    Age: Byte;
    constructor Create(const AForename, ASurname: string; AAge: Byte);
  end;

constructor TPersonRec.Create(const AForename, ASurname: string; AAge: Byte);
begin
  Forename := AForename;
  Surname := ASurname;
  Age := AAge;
end;

var
  People: TArray<TPersonRec>;
  Person: TPersonRec;
begin
  //construct an unsorted array
  SetLength(People, 6);
  People[0] := TPersonRec.Create('Jill', 'Smith', 32);
  People[1] := TPersonRec.Create('Robert', 'Jones', 28);
  People[2] := TPersonRec.Create('Aziz', 'Howayek', 28);
  People[3] := TPersonRec.Create('Anjum', 'Smith', 32);
  People[4] := TPersonRec.Create('Anne', 'Jacobs', 41);
  People[5] := TPersonRec.Create('Raza', 'Saad', 27);

  //print the array out
  WriteLn('Unsorted...');
  for Person in People do
    WriteLn('  Age ', Person.Age, ': ', Person.Surname, ', ', Person.Forename);
  WriteLn;

  //sort using a custom comparer constructed in place
  TArray.Sort<TPersonRec>(People, TComparer<TPersonRec>.Construct(
    function (const L, R: TPersonRec): Integer
    begin
      Result := L.Age - R.Age;
      if Result <> 0 then Exit;
      Result := CompareStr(L.Surname, R.Surname, loUserLocale);
      if Result = 0 then Result := CompareStr(L.Forename, R.Forename, loUserLocale);
    end));

  //print the now sorted array out
  WriteLn('Sorted by age, then surname, then forename...');
  for Person in People do
    WriteLn('  Age ', Person.Age, ': ', Person.Surname, ', ', Person.Forename);

  WriteLn;
  Write('Press ENTER to exit...');
  Readln;
end.

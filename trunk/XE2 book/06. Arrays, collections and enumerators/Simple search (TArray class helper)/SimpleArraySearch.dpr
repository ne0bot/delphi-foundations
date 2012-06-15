program SimpleArraySearch;
{
  TArray of Generics.Collections only supports searching a sorted array. This
  demo implements a class helper to fix that.
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Generics.Collections,
  CCR.Generics.SimpleArraySearch in 'CCR.Generics.SimpleArraySearch.pas';

var
  FoundIndex: Integer;
  Items: TArray<string>;
  S: string;
begin
  Items := TArray<string>.Create('first', 'second', 'third', 'fourth', 'fifth', 'sixth');
  WriteLn('Array:');
  for S in Items do
    Writeln('  ', S);
  WriteLn;
  Writeln('Using TArray.BinarySearch (won''t work, as array isn''t sorted):');
  if TArray.BinarySearch<string>(Items, 'fourth', FoundIndex) then
    Writeln(' Found at index ', FoundIndex)
  else
    Writeln(' Didn''t find it');
  WriteLn;
  Writeln('Using TArray.FindFirst (added using a class helper):');
  if TArray.FindFirst<string>(Items, 'fourth', FoundIndex) then
    Writeln(' Found at index ', FoundIndex)
  else
    Writeln(' Didn''t find it');
  WriteLn;
  Write('Press ENTER to exit...');
  Readln;
end.

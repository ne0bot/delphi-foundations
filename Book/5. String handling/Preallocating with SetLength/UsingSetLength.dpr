program UsingSetLength;
{
  Simple demo of the speed advantage of pre-allocating a string with SetLength.
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.Diagnostics;

function ReverseString_NotUsingSetLength(const S: string): string;
var
  Ch: Char;
begin
  Result := '';
  for Ch in S do
    Result := Result + Ch;
end;

function ReverseString_UsingSetLength(const S: string): string;
var
  I, Len: Integer;
begin
  Len := Length(S);
  SetLength(Result, Len);
  for I := 1 to Len do
    Result[Len - I + 1] := S[I];
end;

var
  Stopwatch: TStopwatch;
  TestString: string;
begin
  TestString := StringOfChar('#', $FFFFFF);
  WriteLn(Format('Testing with a string of %.0n characters in length...',
    [Int(Length(TestString))]));
  Stopwatch := TStopwatch.StartNew;
  ReverseString_NotUsingSetLength(TestString);
  Stopwatch.Stop;
  WriteLn('Not using Setlength took ', Stopwatch.ElapsedMilliseconds, 'ms');
  Stopwatch.Reset;
  Stopwatch.Start;
  ReverseString_UsingSetLength(TestString);
  Stopwatch.Stop;
  WriteLn('Using SetLength took ', Stopwatch.ElapsedMilliseconds, 'ms');
  Readln;
end.

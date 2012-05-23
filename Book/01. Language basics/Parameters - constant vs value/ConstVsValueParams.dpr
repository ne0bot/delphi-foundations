program ConstVsValueParams;
{
  Small demo to give an indication of the relative performance of a constant over a
  regular paramater for a sizeable value type. Note the test is written to emphasise
  the potential difference!
}
{$APPTYPE CONSOLE}

uses
  System.Diagnostics;

//define a large value type
type
  TBigData = array[Word] of Byte;

//each test function will just assign the first element of the argument to the result
function TestConstParam(const Data: TBigData): Byte;
begin
  Result := Data[0];
end;

function TestValueParam(Data: TBigData): Byte;
begin
  Result := Data[0];
end;

//time each test function being called lots of times, and output the results
const
  NumTimes = 100000;
var
  Data: TBigData;
  I, Round: Integer;
  RetVal: Byte;
  Stopwatch: TStopwatch;
begin
  for Round := 1 to 3 do
  begin
    WriteLn('Round ', Round, '...');
    Stopwatch := TStopwatch.StartNew;
    for I := 1 to NumTimes do
      RetVal := TestConstParam(Data);
    Stopwatch.Stop;
    Writeln('Const param: ', Stopwatch.ElapsedMilliseconds, 'ms');
    Stopwatch := TStopwatch.StartNew;
    for I := 1 to NumTimes do
      RetVal := TestValueParam(Data);
    Stopwatch.Stop;
    Writeln('Value param: ', Stopwatch.ElapsedMilliseconds, 'ms');
    WriteLn('');
  end;
  Write('Press ENTER to exit...');
  Readln;
end.

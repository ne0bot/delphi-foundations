program TimingDemo;
{
  Simple demo of TStopwatch usage. Since TStopwatch is a record type, it does not require explicit
  destruction.
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.TimeSpan, System.Diagnostics;

const
  LoopToNum = $FFFF;

function AddNums: Integer;
var
  I, J: Integer;
begin
  for I := 1 to LoopToNum do
    for J := 1 to LoopToNum do
      Result := I + J;
end;

function MultiplyNums: Integer;
var
  I, J: Integer;
begin
  for I := 1 to LoopToNum do
    for J := 1 to LoopToNum do
      Result := I * J;
end;

type
  TTestFunc = function : Integer;

procedure DoTiming(const Desc: string; const TestFunc: TTestFunc);
var
  Stopwatch: TStopwatch;
begin
  WriteLn('Performing test...');
  Stopwatch := TStopwatch.StartNew;
  TestFunc;
  Stopwatch.Stop;
  WriteLn(Format('%s took %n seconds', [Desc, Stopwatch.Elapsed.TotalSeconds]));
  WriteLn;
end;

begin
  DoTiming('Adding numbers', AddNums);
  DoTiming('Multiplying numbers', MultiplyNums);
  Write('Press ENTER to exit...');
  Readln;
end.

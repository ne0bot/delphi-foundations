program SetsAndBitwiseOps;
{
  This demo compares the built-in set syntax with the syntax for manual 'sets' using bitwise
  operators. The latter is how you would emulate Delphi/Pascal sets in languages like C, and is a
  style occasionally used even in Delphi.
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils;

const
  mcFirst = 1;
  mcSecond = 2;
  mcThird = 4;
  mcFourth = 8;

type
  TMyEnum = (meFirst, meSecond, meThird, meFourth);
  TMySet = set of TMyEnum;

var
  UsingSet: TMySet;
  UsingConsts: LongWord;
begin
  //direct assignment
  UsingSet := [meSecond, meThird];
  UsingConsts := mcSecond or mcThird;
  WriteLn('Equal? ', Byte(UsingSet) = UsingConsts);
  //subtraction
  Exclude(UsingSet, meSecond);
  UsingConsts := UsingConsts and not mcSecond;
  WriteLn('Equal? ', Byte(UsingSet) = UsingConsts);
  //addition
  Include(UsingSet, meFourth);
  UsingConsts := UsingConsts or mcFourth;
  WriteLn('Equal? ', Byte(UsingSet) = UsingConsts);
  //membership test
  if meThird in UsingSet then WriteLn('meThird is in');
  if UsingConsts and mcThird <> 0 then WriteLn('mcThird is in');
  ReadLn;
end.

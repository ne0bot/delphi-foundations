program StrConcatPerfTest;
{
  Simple string concatenation speed test that puts the naive method of using the plus
  operator up against TStringBuilder, TStringList and TMemoryStream. The plus operator
  easily beats the first too, and is a bit quicker than TMemoryStream as well, despite
  the test code setting all their respective Capacity properties up front to avoid the
  need for internal memory reallocations in the middle of the loop. On my machine,
  targeting 64 bit doesn't change things much, though the plus operator is now even
  quicker and TStringList worse!
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.TimeSpan,
  System.Diagnostics;

const
  LoopToValue = 5500000;
  StrToConcat = 'Sometimes, doing things the easiest way is actually the fastest way too.';

function TestStringConcat: string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to LoopToValue do
    Result := Result + StrToConcat + sLineBreak;
end;

function TestStringBuilder: string;
var
  Builder: TStringBuilder;
  I: Integer;
begin
  { Create the string builder, giving it a capacity big enough to take all the data we
    will actually be adding. }
  Builder := TStringBuilder.Create(LoopToValue * Length(StrToConcat + sLineBreak));
  try
    for I := 1 to LoopToValue do
      Builder.AppendLine(StrToConcat);
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

function TestStringList: string;
var
  List: TStringList;
  I: Integer;
begin
  List := TStringList.Create;
  try
    List.Capacity := LoopToValue;
    for I := 1 to LoopToValue do
      List.Add(StrToConcat);
    Result := List.Text;
  finally
    List.Free;
  end;
end;

function TestMemoryStream: string;
var
  S: string;
  Stream: TMemoryStream;
  I: Integer;
begin
  Stream := TMemoryStream.Create;
  try
    { If we won't pre-allocate, we'll get an out of memory exception in a 32 bit build
      given the way TMemoryStream increases its capacity. }
    Stream.Size := LoopToValue * ByteLength(StrToConcat + sLineBreak);
    for I := 1 to LoopToValue do
    begin
      S := StrToConcat + sLineBreak;
      Stream.WriteBuffer(S[1], ByteLength(S));
    end;
    SetString(Result, PChar(Stream.Memory), Stream.Size div SizeOf(Char));
  finally
    Stream.Free;
  end;
end;

type
  TTestFunc = function : string;

procedure DoTiming(const Desc: string; const TestFunc: TTestFunc);
var
  Output: string;
  Stopwatch: TStopwatch;
begin
  Stopwatch := TStopwatch.StartNew;
  Output := TestFunc;
  Stopwatch.Stop;
  WriteLn(Format('%s took %n seconds creating a %d character string',
    [Desc, Stopwatch.Elapsed.TotalSeconds, Length(Output)]));
end;

begin
  DoTiming('Plus operator', TestStringConcat);
  DoTiming('TMemoryStream', TestMemoryStream);
  DoTiming('TStringBuilder', TestStringBuilder);
  DoTiming('TStringList', TestStringList);
  Write('Press ENTER to exit...');
  ReadLn;
end.

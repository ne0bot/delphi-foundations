program BufferedInputDemo;
{
  Benchmarks a custom proxy stream class that buffers input from an underlying
  stream. The test is to allocate a dummy file of 10MB that is then read in two
  byte chunks. This is designed to show off the speed benefits the proxy can
  provide. Be aware that in a real world situation that may not be the case -
  indeed, a proxy can actually slow things down. The key to this benchmark's
  results is how the data is read in so small a sized pieces.
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Diagnostics,
  BufferedStreams in 'BufferedStreams.pas';

var
  TestFile: string;

procedure RunTest(const TestName: string; Stream: TStream);
var
  NumComplete: Integer;
  Value: Int16;
  Stopwatch: TStopwatch;
begin
  WriteLn('Testing ', TestName, '...');
  NumComplete := 0;
  try
    Stopwatch := TStopwatch.StartNew;
    while Stream.Read(Value, 2) = 2 do Inc(NumComplete);
    Stopwatch.Stop;
    Writeln('  Read ', NumComplete, ' two-byte chunks in ',
      Stopwatch.ElapsedMilliseconds, 'ms');
  finally
    Stream.Free;
  end;
end;

var
  I: Integer;
begin
  //allocate a dummy temporary file of 10MB in size
  TestFile := TPath.GetTempFileName;
  with TFileStream.Create(TestFile, fmOpenWrite) do
  try
    Size := $A00000;
  finally
    Free;
  end;
  for I := 1 to 3 do
  begin
    WriteLn('*** ROUND ', I, ' ***');
    RunTest('TFileStream directly', TFileStream.Create(TestFile, fmOpenRead));
    RunTest('TBufferedInputStream', TBufferedInputStream.Create(
      TFileStream.Create(TestFile, fmOpenRead), True));
    WriteLn;
  end;
  DeleteFile(TestFile);
  Write('Press ENTER to exit...');
  ReadLn;
end.

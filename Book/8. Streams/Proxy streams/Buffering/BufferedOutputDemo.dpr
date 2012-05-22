program BufferedOutputDemo;
{
  This program benchmarks an output stream buffered with a proxy against an
  unbuffered output stream. As with the buffered input demo, the results only
  go as they do because the test is designed to show a big difference - if the
  thing writing to the stream composes everything in memory first, then a
  buffering proxy will only slow things down.
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.Classes, System.IOUtils, System.Diagnostics,
  BufferedStreams in 'BufferedStreams.pas';

procedure RunTest(const TestName: string; Stream: TStream);
const
  ChunksToWrite = $200000;
var
  I: Integer;
  Stopwatch: TStopwatch;
begin
  WriteLn('Testing ', TestName, '...');
  try
    Stopwatch := TStopwatch.StartNew;
    for I := 2 to ChunksToWrite do
      Stream.WriteBuffer(I, 2);
    Stopwatch.Stop;
    WriteLn('  Written ', ChunksToWrite, ' chunks totalling ',
      Stream.Size, ' bytes in ', Stopwatch.ElapsedMilliseconds, 'ms');
  finally
    Stream.Free;
  end;
end;

var
  I: Integer;
  TestFile: string;
begin
  TestFile := TPath.GetTempFileName;
  for I := 1 to 3 do
  begin
    WriteLn('*** ROUND ', I, ' ***');
    RunTest('TFileStream directly', TFileStream.Create(TestFile, fmCreate));
    RunTest('TBufferedOutputStream', TBufferedOutputStream.Create(
      TFileStream.Create(TestFile, fmCreate), True));
    WriteLn;
  end;
  DeleteFile(TestFile);
  Write('Press ENTER to exit...');
  ReadLn;
end.

program NonList;
{
  Demonstrates a custom enumerator that doesn't work on a list class: instead, the
  enumerater parses a file or stream in fixed sized chunks. As a parser, it's pretty
  pointless - you would do better to just call TStream.Read directly - but the aim is
  to show the for/in construct doesn't necessarily have to map onto a for/to one.
  A more practical example would parse a specific chunk-based format.
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  FileChunkParser in 'FileChunkParser.pas';

var
  Chunk: TBytes;
  TestFile: string;
begin
  //find the path to the test file
  TestFile := ExtractFilePath(ParamStr(0)) + '..' + PathDelim + '..' + PathDelim + 'Test.txt';
  if not FileExists(TestFile) then
  begin
    WriteLn('ERROR: couldn''t find "Test.txt"');
    Exit;
  end;
  //enumerate its contents in chunks of 8KB
  for Chunk in ParseChunks(TestFile, 1024 * 8) do
  begin
    Write('Press ENTER to output the next chunk...');
    ReadLn;
    WriteLn;
    WriteLn(StringOf(Chunk));
    WriteLn;
  end;
  Write('Press ENTER to exit...');
  ReadLn;
end.

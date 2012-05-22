program BasicTStreamUsage;
{
  Simple demo of seeking and reading from a stream. The task chosen is reading
  from the EXE file's header. See MSDN for information about the structures
  involved (http://msdn.microsoft.com/en-us/library/ms680313.aspx).            }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.DateUtils;

function PEHeaderDateTime(const FileName: string): TDateTime;
var
  DOSHeader: TImageDosHeader;
  NTHeader: TImageNtHeaders;
  Stream: TFileStream;
begin
  Result := 0;
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Stream.ReadBuffer(DOSHeader, SizeOf(DOSHeader));
    if DOSHeader.e_magic <> IMAGE_DOS_SIGNATURE then
      raise EParserError.Create('Invalid PE file - DOS header not found.');
    Stream.Seek(DOSHeader._lfanew, soBeginning);
    Stream.ReadBuffer(NTHeader, SizeOf(NTHeader));
    if NTHeader.Signature <> IMAGE_NT_SIGNATURE then
      raise EParserError.Create('Invalid PE file - NT header not found.');
    Result := TTimeZone.Local.ToLocalTime(UnixToDateTime(
      NTHeader.FileHeader.TimeDateStamp));
  finally
    Stream.Free;
  end;
end;

function GetWindowsPath: string;
var
  Buffer: array[0..MAX_PATH] of Char;
  Len: Integer;
begin
  Len := GetWindowsDirectory(Buffer, MAX_PATH);
  if Buffer[Len - 1] <> PathDelim then
  begin
    Buffer[Len] := PathDelim;
    Inc(Len);
  end;
  SetString(Result, Buffer, Len);
end;

begin
  try
    Write('This EXE file was compiled on ');
    Writeln(FormatDateTime('dddddd "at" t', PEHeaderDateTime(ParamStr(0))));
    Write('This computer''s Explorer.exe was compiled on ');
    Writeln(FormatDateTime('dddddd "at" t', PEHeaderDateTime(GetWindowsPath + 'explorer.exe')));
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
  WriteLn;
  Write('Press ENTER to exit...');
  Readln;
end.

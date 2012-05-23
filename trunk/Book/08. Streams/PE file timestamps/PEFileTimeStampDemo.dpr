program PEFileTimeStampDemo;
{
  Small demo of seeking and reading from a stream. The task chosen is reading
  from the EXE file's header. See MSDN for information about the structures
  involved (http://msdn.microsoft.com/en-us/library/ms680313.aspx).            }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.DateUtils;

function PEHeaderDateTimeUTC(const FileName: string): TDateTime;
var
  DOSHeader: TImageDosHeader;
  NTHeader: TImageNtHeaders;
  Stream: TFileStream;
begin
  Result := 0;
  //open a file stream in read mode
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    //read the DOS header and validate it
    Stream.ReadBuffer(DOSHeader, SizeOf(DOSHeader));
    if DOSHeader.e_magic <> IMAGE_DOS_SIGNATURE then
      raise EParserError.Create('Invalid PE file - DOS header not found.');
    //seek to the NT header, read and validate it
    Stream.Seek(DOSHeader._lfanew, soBeginning);
    Stream.ReadBuffer(NTHeader, SizeOf(NTHeader));
    if NTHeader.Signature <> IMAGE_NT_SIGNATURE then
      raise EParserError.Create('Invalid PE file - NT header not found.');
    //read off the timestamp and convert it to a TDateTime
    Result := UnixToDateTime(NTHeader.FileHeader.TimeDateStamp);
  finally
    Stream.Free;
  end;
end;

function PEHeaderDateTime(const FileName: string): TDateTime; inline;
begin
  Result := TTimeZone.Local.ToLocalTime(PEHeaderDateTimeUTC(FileName));
end;

procedure PatchPEHeaderDateTime(const FileName: string;
  NewValue: TDateTime; IsLocal: Boolean = True);
var
  DOSHeader: TImageDosHeader;
  NTHeader: TImageNtHeaders;
  Stream: TFileStream;
begin
  //convert the new date to UTC/GMT if necessary
  if IsLocal then NewValue := TTimeZone.Local.ToUniversalTime(NewValue);
  //open a file stream in read/write mode
  Stream := TFileStream.Create(FileName, fmOpenReadWrite);
  try
    //read the DOS header and validate it
    Stream.ReadBuffer(DOSHeader, SizeOf(DOSHeader));
    if DOSHeader.e_magic <> IMAGE_DOS_SIGNATURE then
      raise EParserError.Create('Invalid PE file - DOS header not found.');
    //seek to the NT header, read and validate it
    Stream.Seek(DOSHeader._lfanew, soBeginning);
    Stream.ReadBuffer(NTHeader, SizeOf(NTHeader));
    if NTHeader.Signature <> IMAGE_NT_SIGNATURE then
      raise EParserError.Create('Invalid PE file - NT header not found.');
    //update the structure just read in
    NTHeader.FileHeader.TimeDateStamp := DateTimeToUnix(NewValue);
    //seek back on ourselves
    Stream.Seek(-SizeOf(NTHeader), soCurrent);
    //write the revised record structure
    Stream.WriteBuffer(NTHeader, SizeOf(NTHeader));
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

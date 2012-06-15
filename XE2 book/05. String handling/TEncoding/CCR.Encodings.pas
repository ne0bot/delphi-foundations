unit CCR.Encodings;
{
  This unit provides a custom encoding detection routine plus three example TEncoding
  descendants.

  The detection routine looks for the BOMs for UTF-8, UTF-32 LE, UTF-32 BE, UTF-16 LE
  and UTF-16 BE; on none being found, the RTL's DetectUTF8Encoding function is called to
  determine whether the file or stream forms valid UTF-8. In contrast, the RTL's stock
  detection method looks only for the BOMs of UTF-8, UTF-16 LE and UTF-16 BE, ignoring
  the possibility of both UTF-32 and (on Windows) BOM-less UTF-8.

  Regarding the custom TEncoding descendants, the first is a variant of TUTF8Encoding
  that optionally won't output a BOM and/or return return nothing on an invalid
  character sequence (returning nothing is what the stock TUTF8Encoding does). The other
  two are TUTF32Encoding and TBigEndianUTF32Encoding implementations that delegate to
  the RTL's own UCS4StringToUnicodeString and WideCharToUCS4String functions to do the
  hard work. Notice the Clone method needs to be overridden in a custom TEncoding
  descendant!
}
interface

uses
  System.SysUtils, System.Classes;

type
  TEncodingOptions = set of (eoAllowInvalidChars, eoUseBOM);

  TUTF8EncodingEx = class(TUTF8Encoding)
  strict private
    FOptions: TEncodingOptions;
  public
    constructor Create(AOptions: TEncodingOptions);
    function Clone: TEncoding; override;
    function GetPreamble: TBytes; override;
    property Options: TEncodingOptions read FOptions;
  end;

  TUTF32Encoding = class(TEncoding)
  strict private
    FOptions: TEncodingOptions;
  strict protected
    function GetByteCount(Chars: PChar; CharCount: Integer): Integer; overload; override;
    function GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetCharCount(Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetChars(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer; overload; override;
    function GetEncodingName: string; override;
  public
    constructor Create(AOptions: TEncodingOptions = [eoAllowInvalidChars, eoUseBOM]);
    function Clone: TEncoding; override;
    function GetMaxByteCount(CharCount: Integer): Integer; override;
    function GetMaxCharCount(ByteCount: Integer): Integer; override;
    function GetPreamble: TBytes; override;
    property Options: TEncodingOptions read FOptions;
  end;

  TBigEndianUTF32Encoding = class(TUTF32Encoding)
  strict protected
    function GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetChars(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer; overload; override;
    function GetEncodingName: string; override;
  public
    function Clone: TEncoding; override;
    function GetPreamble: TBytes; override;
  end;

type
  TDetectedEncodingType = (deUnrecognized, deUTF8, deUTF8NoBOM, deUTF16LE, deUTF16BE,
    deUTF32LE, deUTF32BE);

function DetectEncoding(AStream: TStream): TDetectedEncodingType; overload;
function DetectEncoding(const AFileName: string): TDetectedEncodingType; overload;

implementation

uses WideStrUtils;

function DetectEncoding(AStream: TStream): TDetectedEncodingType; overload;
var
  BOM: array[1..2] of UInt16;
  BytesRead: Integer;
  PotentialUTF8: UTF8String;
begin
  Result := deUnrecognized;
  //load the first few bytes, restoring the stream's position once we've done that
  BytesRead := AStream.Read(BOM, SizeOf(BOM));
  AStream.Seek(-BytesRead, soCurrent);
  if BytesRead < SizeOf(BOM) then Exit;
  //look for valid BOMs
  if CompareMem(@BOM, @sUTF8BOMString, SizeOf(sUTF8BOMString)) then
    Exit(deUTF8);
  case BOM[1] of
    $0000: if BOM[2] = $FFFE then Exit(deUTF32BE);
    $FEFF: if BOM[2] = $0000 then Exit(deUTF32LE) else Exit(deUTF16LE);
    $FFFE: Exit(deUTF16BE);
  end;
  //no matching BOM, so see whether the file as a whole contains valid UTF-8
  SetLength(PotentialUTF8, Integer(AStream.Size - AStream.Position));
  BytesRead := AStream.Read(PotentialUTF8[1], Length(PotentialUTF8));
  AStream.Seek(-BytesRead, soCurrent);
  if DetectUTF8Encoding(PotentialUTF8) in [etUSASCII, etUTF8] then
    Exit(deUTF8NoBOM);
end;

function DetectEncoding(const AFileName: string): TDetectedEncodingType; overload;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := DetectEncoding(Stream);
  finally
    Stream.Free;
  end;
end;

{ TUTF8EncodingEx }

constructor TUTF8EncodingEx.Create(AOptions: TEncodingOptions);
begin
  if eoAllowInvalidChars in AOptions then
    inherited Create(65001, 0, 0)
  else
    inherited Create;
  FOptions := AOptions;
end;

function TUTF8EncodingEx.Clone: TEncoding;
begin
  Result := TUTF8EncodingEx.Create(Options);
end;

function TUTF8EncodingEx.GetPreamble: TBytes;
begin
  if eoUseBOM in Options then
    Result := inherited GetPreamble
  else
    Result := nil;
end;

{ TUTF32Encoding }

constructor TUTF32Encoding.Create(AOptions: TEncodingOptions);
begin
  inherited Create;
  FOptions := AOptions;
end;

function TUTF32Encoding.Clone: TEncoding;
begin
  Result := TUTF32Encoding.Create(Options);
end;

function TUTF32Encoding.GetByteCount(Chars: PChar; CharCount: Integer): Integer;
begin
  Result := CharCount * 4;
end;

function TUTF32Encoding.GetBytes(Chars: PChar; CharCount: Integer;
  Bytes: PByte; ByteCount: Integer): Integer;
var
  UTF32: UCS4String;
begin
  UTF32 := WideCharToUCS4String(Chars, CharCount);
  Result := Length(UTF32) * 4;
  if Result > ByteCount then Result := ByteCount - ByteCount mod 4;
  Move(PUCS4Char(UTF32)^, Bytes^, Result);
end;

function TUTF32Encoding.GetCharCount(Bytes: PByte; ByteCount: Integer): Integer;
begin
  Result := ByteCount div 4;
end;

function TUTF32Encoding.GetChars(Bytes: PByte; ByteCount: Integer;
  Chars: PChar; CharCount: Integer): Integer;
var
  I: Integer;
  UTF32: UCS4String;
  UTF16: string;
begin
  //get the bytes into a UCS4String variable (n.b.: UCS4StringToUnicodeString ignores the last element!)
  Dec(ByteCount, ByteCount mod 4);
  SetLength(UTF32, ByteCount div 4 + 1);
  Move(Bytes^, PUCS4Char(UTF32)^, ByteCount);
  //if not allowing invalid chars, scan through checking for them
  if not (eoAllowInvalidChars in Options) then
    for I := High(UTF32) downto Low(UTF32) do
      if UTF32[I] > $10FFFF then Exit(0);
  //do the conversion
  UTF16 := UCS4StringToUnicodeString(UTF32);
  Result := Length(UTF16);
  if Result > CharCount then Result := CharCount;
  MoveChars(PChar(UTF16)^, Chars^, Result);
end;

function TUTF32Encoding.GetEncodingName: string;
begin
  Result := 'Unicode (UTF-32LE)';
end;

function TUTF32Encoding.GetMaxByteCount(CharCount: Integer): Integer;
begin
  Result := CharCount * 4;
end;

function TUTF32Encoding.GetMaxCharCount(ByteCount: Integer): Integer;
begin
  Result := ByteCount div ByteCount;
end;

function TUTF32Encoding.GetPreamble: TBytes;
begin
  if eoUseBOM in Options then
    Result := TBytes.Create($FF, $FE, $00, $00)
  else
    Result := nil;
end;

{ TBigEndianUTF32Encoding }

function SwapUCS4Char(const Value: UCS4Char): UCS4Char; inline;
begin
  Result :=
    ((Value and $000000FF) shl 24) or
    ((Value and $0000FF00) shl 8) or
    ((Value and $00FF0000) shr 8) or
    ((Value and $FF000000) shr 24);
end;

function TBigEndianUTF32Encoding.Clone: TEncoding;
begin
  Result := TUTF32Encoding.Create(Options);
end;

function TBigEndianUTF32Encoding.GetEncodingName: string;
begin
  Result := 'Unicode (UTF-32BE)';
end;

function TBigEndianUTF32Encoding.GetBytes(Chars: PChar; CharCount: Integer;
  Bytes: PByte; ByteCount: Integer): Integer;
var
  I: Integer;
  UCS4Chars: PUCS4CharArray absolute Bytes;
begin
  Result := inherited GetBytes(Chars, CharCount, Bytes, ByteCount);
  for I := Result div 4 - 1 downto 0 do
    UCS4Chars[I] := SwapUCS4Char(UCS4Chars[I]);
end;

function TBigEndianUTF32Encoding.GetChars(Bytes: PByte; ByteCount: Integer;
  Chars: PChar; CharCount: Integer): Integer;
var
  I: Integer;
  UCS4Chars: PUCS4CharArray absolute Bytes;
begin
  for I := ByteCount div 4 - 1 downto 0 do
    UCS4Chars[I] := SwapUCS4Char(UCS4Chars[I]);
  Result := inherited GetChars(Bytes, ByteCount, Chars, CharCount);
end;

function TBigEndianUTF32Encoding.GetPreamble: TBytes;
begin
  if eoUseBOM in Options then
    Result := TBytes.Create($00, $00, $FE, $FF)
  else
    Result := nil;
end;

end.

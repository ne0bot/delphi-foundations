unit RegistryUtils;
{
  Routines for reading and writing REG_QWORD and REG_MULTI_SZ values from the
  Windows Registry, which TRegistry doesn't directly support.
}
interface

uses
  Winapi.Windows, System.Classes, System.Win.Registry;

function ExpandEnvironmentVariables(const S: string): string;

function ReadInt64FromRegistry(Registry: TRegistry; const ValueName: string): Int64;
procedure ReadStringsFromRegistry(Registry: TRegistry; const ValueName: string;
  Strings: TStrings); overload;
function ReadStringsFromRegistry(Registry: TRegistry; const ValueName: string): TArray<string>; overload;
procedure WriteInt64ToRegistry(Registry: TRegistry; const ValueName: string;
  const Data: Int64);
procedure WriteStringsToRegistry(Registry: TRegistry; const ValueName: string;
  const Strings: array of string); overload;
procedure WriteStringsToRegistry(Registry: TRegistry; const ValueName: string;
  Strings: TStrings); overload; inline;

{$IF NOT DECLARED(REG_QWORD)}
const
  REG_QWORD = 11;
{$IFEND}

implementation

uses
  System.RTLConsts;

function ExpandEnvironmentVariables(const S: string): string;
var
  Len: DWORD;
begin
  Len := ExpandEnvironmentStrings(PChar(S), nil, 0);
  SetLength(Result, Len);
  ExpandEnvironmentStrings(PChar(S), PChar(Result), Len + 1);
end;

function ReadInt64FromRegistry(Registry: TRegistry; const ValueName: string): Int64;
begin
  if Registry.ReadBinaryData(ValueName, Result, SizeOf(Result)) <> SizeOf(Result) then
    raise ERegistryException.CreateResFmt(@SRegGetDataFailed, [ValueName]);
end;

procedure ReadStringsFromRegistry(Registry: TRegistry; const ValueName: string;
  Strings: TStrings);
var
  Chars: array of Char;
  Len: Integer;
  SeekPtr: PChar;
  S: string;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    Len := Registry.GetDataSize(ValueName) div SizeOf(Char);
    if Len < 2 then Exit;
    //add two nulls so will always have valid data
    SetLength(Chars, Len + 2);
    Chars[Len] := #0;
    Chars[Len + 1] := #0;
    Registry.ReadBinaryData(ValueName, Chars[0], Len * SizeOf(Char));
    //scan thru'
    SeekPtr := @Chars[0];
    repeat
      S := SeekPtr;
      if Length(S) = 0 then Break; //we've come to the terminating double null
      Strings.Add(S);
      Inc(SeekPtr, Length(S) + 1);
    until False;
  finally
    Strings.EndUpdate;
  end;
end;

function ReadStringsFromRegistry(Registry: TRegistry; const ValueName: string): TArray<string>;
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    ReadStringsFromRegistry(Registry, ValueName, Strings);
    Result := Strings.ToStringArray;
  finally
    Strings.Free;
  end;
end;

procedure WriteDataToRegistry(Registry: TRegistry; const ValueName: string;
  DataType: DWORD; const Data; DataLen: Integer);
begin
  if RegSetValueEx(Registry.CurrentKey, PChar(ValueName), 0, DataType, @Data,
    DataLen) <> ERROR_SUCCESS then
    raise ERegistryException.CreateResFmt(@SRegSetDataFailed, [ValueName]);
end;

procedure WriteInt64ToRegistry(Registry: TRegistry; const ValueName: string;
  const Data: Int64);
begin
  WriteDataToRegistry(Registry, ValueName, REG_QWORD, Data, SizeOf(Data));
end;

procedure WriteStringsToRegistry(Registry: TRegistry; const ValueName: string;
  const Strings: array of string);
var
  Len: Integer;
  S, ToSave: string;
begin
  Len := 1;
  for S in Strings do
    Inc(Len, Length(S) + 1);
  if Len = 1 then
    ToSave := #0#0
  else
  begin
    SetLength(ToSave, Len);
    ToSave[Len] := #0;
    Len := 1;
    for S in Strings do
    begin
      MoveChars(S[1], ToSave[Len], Length(S) + 1); //1 for the null term
      Inc(Len, Length(S) + 1);
    end;
  end;
  WriteDataToRegistry(Registry, ValueName, REG_MULTI_SZ, ToSave[1],
    Length(ToSave) * SizeOf(Char));
end;

procedure WriteStringsToRegistry(Registry: TRegistry; const ValueName: string;
  Strings: TStrings);
begin
  WriteStringsToRegistry(Registry, ValueName, Strings.ToStringArray);
end;

end.

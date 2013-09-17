{**************************************************************************************}
{                                                                                      }
{ FMX Utilities: TMacPreferencesIniFile                                                }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2013 Chris Rolliston. All Rights Reserved.         }
{                                                                                      }
{**************************************************************************************}

unit CCR.MacPrefsIniFile deprecated;
{
  This unit has been replaced with CCR.PrefsIniFile.Apple (for
  TMacPreferencesIniFile, now TApplePreferencesIniFile) and CCR.PrefsIniFile
  (for CreateUserPreferencesIniFile).
}
interface

uses
  System.SysUtils, System.Classes, System.IniFiles, System.StrUtils
  {$IFDEF MACOS}, Macapi.CoreFoundation{$ENDIF};

type
  TMacPreferencesIniFile = class(TCustomIniFile)
  {$IFDEF MACOS}
  strict private
    FAllKeysCache: TArray<string>;
    FApplicationID: CFStringRef;
    FApplicationIDStr: string;
    FDelimiter: Char;
    FUsingCustomApplicationID: Boolean;
  protected
    function GetAllKeys: TArray<string>;
    procedure WriteValue(const Key: string; Value: CFPropertyListRef;
      ReleaseValueAfterWrite: Boolean = False); overload;
  public
    const DefaultDelimiter = ':';
    constructor Create(const ApplicationID: string = '');
    destructor Destroy; override;
    procedure DeleteKey(const Section, Ident: string); override;
    procedure EraseSection(const Section: string); override;
    function ReadBinaryStream(const Section, Ident: string; Stream: TStream): Integer; override;
    function ReadDateTime(const Section, Ident: string; Default: TDateTime): TDateTime; override;
    function ReadDate(const Section, Ident: string; Default: TDateTime): TDateTime; override;
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSections(const ParentSection: string; Strings: TStrings); override;
    procedure ReadSubSections(const ParentSection: string; Strings: TStrings;
      Recurse: Boolean); override;
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; override;
    function ReadFloat(const Section, Ident: string; Default: Double): Double; override;
    function ReadInteger(const Section, Ident: string; Default: Integer): Integer; override;
    function ReadString(const Section, Ident, Default: string): string; override;
    function ReadValue(const Section, Ident: string): CFPropertyListRef;
    procedure UpdateFile; override;
    function ValueExists(const Section, Ident: string): Boolean; override;
    procedure WriteBinaryStream(const Section: string; const Ident: string; Stream: TStream); override;
    procedure WriteBool(const Section, Ident: string; Value: Boolean); override;
    procedure WriteDateTime(const Section, Ident: string; Value: TDateTime); override;
    procedure WriteDate(const Section, Ident: string; Value: TDateTime); override;
    procedure WriteInteger(const Section: string; const Ident: string; Value: Int32); override;
    procedure WriteFloat(const Section: string; const Ident: string; Value: Double); override;
    procedure WriteString(const Section, Ident, Value: string); override;
    procedure WriteValue(const Section, Ident: string; Value: CFPropertyListRef;
      ReleaseValueAfterWrite: Boolean = False); overload;
    property ApplicationID: string read FApplicationIDStr;
    property Delimiter: Char read FDelimiter write FDelimiter default DefaultDelimiter;
  {$ELSE}
  public
    constructor Create; platform;
  {$ENDIF}
  end deprecated 'Use TApplePreferencesIniFile from CCR.PrefsIniFile.Apple instead';

{$SCOPEDENUMS ON}
  TWinLocation = (IniFile, Registry);

function CreateUserPreferencesIniFile(AWinLocation: TWinLocation = TWinLocation.Registry): TCustomIniFile; deprecated 'Use the version in CCR.PrefsIniFile instead';

implementation

{$IFDEF MSWINDOWS}
uses Winapi.Windows, System.Win.Registry;
{$ENDIF}
{$IFDEF MACOS}
uses System.DateUtils;

function CFDateCreateFromTDateTime(const DateTime: TDateTime): CFDateRef;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
  GregRec: CFGregorianDate;
  TZ: CFTimeZoneRef;
begin
  DecodeDateTime(DateTime, Year, Month, Day, Hour, Min, Sec, MSec);
  GregRec.year := Year;
  GregRec.month := Month;
  GregRec.day := Day;
  GregRec.hour := Hour;
  GregRec.minute := Min;
  GregRec.second := Sec + MSec / 1000;
  TZ := CFTimeZoneCopyDefault;
  Result := CFDateCreate(nil, CFGregorianDateGetAbsoluteTime(GregRec, TZ));
  CFRelease(TZ);
end;

function CFDateGetAsTDateTime(Date: CFDateRef): TDateTime;
var
  GregRec: CFGregorianDate;
  TZ: CFTimeZoneRef;
begin
  TZ := CFTimeZoneCopyDefault;
  GregRec := CFAbsoluteTimeGetGregorianDate(CFDateGetAbsoluteTime(Date), TZ);
  CFRelease(TZ);
  Result := EncodeDateTime(GregRec.year, GregRec.month, GregRec.day,
    GregRec.hour, GregRec.minute, Trunc(GregRec.second), Trunc(Frac(GregRec.second) * 1000));
end;

function CFNumberGetAsDouble(Number: CFNumberRef): Double; inline;
begin
  CFNumberGetValue(Number, kCFNumberFloat64Type, @Result)
end;

function CFNumberGetAsInt32(Number: CFNumberRef): Int32; inline;
begin
  CFNumberGetValue(Number, kCFNumberSInt32Type, @Result)
end;

function CFStringCreate(const S: string): CFStringRef; inline;
begin
  Result := CFStringCreateWithCharacters(nil, PChar(S), Length(S));
end;

function CFStringCreateNoCopy(const S: string): CFStringRef; inline;
begin
  Result := CFStringCreateWithCharactersNoCopy(nil, PChar(S), Length(S),
    kCFAllocatorNull);
end;

function CFStringGetValue(const CFStr: CFStringRef): string;
var
  Range: CFRange;
begin
  if CFStr = nil then Exit('');
  Range.location := 0;
  Range.length := CFStringGetLength(CFStr);
  SetLength(Result, Range.length);
  CFStringGetCharacters(CFStr, Range, PChar(Result));
end;

{ TMacPreferencesIniFile }

constructor TMacPreferencesIniFile.Create(const ApplicationID: string = '');
var
  BundleHandle: CFBundleRef;
begin
  FUsingCustomApplicationID := (ApplicationID <> '');
  if FUsingCustomApplicationID then
  begin
    FApplicationID := CFStringCreate(ApplicationID);
    FApplicationIDStr := ApplicationID;
  end
  else
  begin
    FApplicationID := kCFPreferencesCurrentApplication;
    //get something sensible for the inherited FileName property
    BundleHandle := CFBundleGetMainBundle;
    if BundleHandle <> nil then
      FApplicationIDStr := CFStringGetValue(CFBundleGetIdentifier(BundleHandle));
    if FApplicationIDStr = '' then
      FApplicationIDStr := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  end;
  inherited Create(IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) +
    'Library/Preferences/' + FApplicationIDStr + '.plist');
  FDelimiter := DefaultDelimiter;
end;

destructor TMacPreferencesIniFile.Destroy;
begin
  UpdateFile;
  if FUsingCustomApplicationID then CFRelease(FApplicationID);
  inherited;
end;

procedure TMacPreferencesIniFile.DeleteKey(const Section, Ident: string);
begin
  WriteValue(Section, Ident, nil);
end;

procedure TMacPreferencesIniFile.EraseSection(const Section: string);
var
  Key, PrefixToFind: string;
begin
  PrefixToFind := Section + Delimiter;
  for Key in GetAllKeys do
    if StartsStr(PrefixToFind, Key) then WriteValue(Key, nil);
end;

function TMacPreferencesIniFile.GetAllKeys: TArray<string>;
var
  I: Integer;
  Keys: CFArrayRef;
begin
  if FAllKeysCache = nil then
  begin
    Keys := CFPreferencesCopyKeyList(FApplicationID, kCFPreferencesCurrentUser, kCFPreferencesAnyHost);
    if Keys <> nil then
      try
        SetLength(FAllKeysCache, CFArrayGetCount(Keys));
        for I := 0 to Length(FAllKeysCache) - 1 do
          FAllKeysCache[I] := CFStringGetValue(CFArrayGetValueAtIndex(Keys, I));
      finally
        CFRelease(Keys);
      end;
  end;
  Result := FAllKeysCache;
end;

function TMacPreferencesIniFile.ReadBinaryStream(const Section, Ident: string;
  Stream: TStream): Integer;
var
  Value: CFPropertyListRef;
begin
  Value := ReadValue(Section, Ident);
  if Value = nil then Exit(0);
  try
    if CFGetTypeID(Value) <> CFDataGetTypeID then Exit(0);
    Result := CFDataGetLength(Value);
    Stream.WriteBuffer(CFDataGetBytePtr(Value)^, Result);
  finally
    CFRelease(Value);
  end;
end;

function TMacPreferencesIniFile.ReadBool(const Section, Ident: string;
  Default: Boolean): Boolean;
var
  TypeID: CFTypeID;
  Value: CFPropertyListRef;
begin
  Value := ReadValue(Section, Ident);
  if Value = nil then
    Result := Default
  else
    try
      TypeID := CFGetTypeID(Value);
      if TypeID = CFBooleanGetTypeID then
        Result := CFBooleanGetValue(Value)
      else if TypeID = CFStringGetTypeID then
        Result := StrToBoolDef(CFStringGetValue(Value), Default)
      else if TypeID = CFNumberGetTypeID then
        Result := (CFNumberGetAsInt32(Value) <> 0)
      else
        Result := Default;
    finally
      CFRelease(Value);
    end;
end;

function TMacPreferencesIniFile.ReadDate(const Section, Ident: string;
  Default: TDateTime): TDateTime;
begin
  Result := Int(ReadDateTime(Section, Ident, Default));
end;

function TMacPreferencesIniFile.ReadDateTime(const Section, Ident: string;
  Default: TDateTime): TDateTime;
var
  TypeID: CFTypeID;
  Value: CFPropertyListRef;
begin
  Value := ReadValue(Section, Ident);
  if Value = nil then
    Result := Default
  else
    try
      TypeID := CFGetTypeID(Value);
      if TypeID = CFDateGetTypeID then
        Result := CFDateGetAsTDateTime(Value)
      else if TypeID = CFStringGetTypeID then
        Result := StrToDateTimeDef(CFStringGetValue(Value), Default)
      else
        Result := Default;
    finally
      CFRelease(Value);
    end;
end;

function TMacPreferencesIniFile.ReadFloat(const Section, Ident: string;
  Default: Double): Double;
var
  TypeID: CFTypeID;
  Value: CFPropertyListRef;
begin
  Value := ReadValue(Section, Ident);
  if Value = nil then
    Result := Default
  else
    try
      TypeID := CFGetTypeID(Value);
      if TypeID = CFNumberGetTypeID then
        Result := CFNumberGetAsDouble(Value)
      else if TypeID = CFStringGetTypeID then
        Result := StrToFloatDef(CFStringGetValue(Value), Default)
      else if TypeID = CFBooleanGetTypeID then
        Result := Ord(CFBooleanGetValue(Value))
      else
        Result := Default;
    finally
      CFRelease(Value);
    end;
end;

function TMacPreferencesIniFile.ReadInteger(const Section, Ident: string;
  Default: Integer): Integer;
var
  TypeID: CFTypeID;
  Value: CFPropertyListRef;
begin
  Value := ReadValue(Section, Ident);
  if Value = nil then
    Result := Default
  else
    try
      TypeID := CFGetTypeID(Value);
      if TypeID = CFNumberGetTypeID then
        Result := CFNumberGetAsInt32(Value)
      else if TypeID = CFStringGetTypeID then
        Result := StrToIntDef(CFStringGetValue(Value), Default)
      else if TypeID = CFBooleanGetTypeID then
        Result := Ord(CFBooleanGetValue(Value))
      else
        Result := Default;
    finally
      CFRelease(Value);
    end;
end;

procedure TMacPreferencesIniFile.ReadSection(const Section: string; Strings: TStrings);
var
  Key, PrefixToFind, S: string;
begin
  if Section <> '' then
    PrefixToFind := Section + Delimiter
  else
    PrefixToFind := '';
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for Key in GetAllKeys do
      if StartsStr(PrefixToFind, Key) then
      begin
        S := Copy(Key, Length(PrefixToFind) + 1, MaxInt);
        if (S <> '') and (Pos(Delimiter, S) = 0) then //igore sub-sections
          Strings.Add(S)
      end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TMacPreferencesIniFile.ReadSections(Strings: TStrings);
begin
  ReadSections('', Strings);
end;

procedure TMacPreferencesIniFile.ReadSections(const ParentSection: string; Strings: TStrings);
begin
  ReadSubSections(ParentSection, Strings, False);
end;

procedure TMacPreferencesIniFile.ReadSubSections(const ParentSection: string;
  Strings: TStrings; Recurse: Boolean);
var
  Added: TStringHash;

  procedure CheckAddSection(const Section: string);
  begin
    if (Section = '') or (Added.ValueOf(Section) = 42) then Exit;
    Strings.Add(Section);
    Added.Add(Section, 42);
  end;
var
  I: Integer;
  Key, PrefixToFind, S: string;
begin
  if ParentSection <> '' then
    PrefixToFind := ParentSection + Delimiter
  else
    PrefixToFind := '';
  Added := nil;
  Strings.BeginUpdate;
  try
    Strings.Clear;
    Added := TStringHash.Create;
    for Key in GetAllKeys do
    begin
      if PrefixToFind = '' then
        CheckAddSection(Copy(Key, 1, Pos(Delimiter, Key) - 1))
      else
      begin
        if not StartsStr(PrefixToFind, Key) then Continue;
        S := Copy(Key, Length(PrefixToFind) + 1, MaxInt);
        I := 0;
        repeat
          I := PosEx(Delimiter, S, I + 1);
          if I <= 1 then Break;
          CheckAddSection(Copy(S, 1, I - 1));
        until not Recurse;
      end;
    end;
  finally
    Strings.EndUpdate;
    Added.Free;
  end;
end;

procedure TMacPreferencesIniFile.ReadSectionValues(const Section: string; Strings: TStrings);
var
  Ident: string;
  I: Integer;
begin
  Strings.BeginUpdate;
  try
    ReadSection(Section, Strings);
    for I := 0 to Strings.Count - 1 do
    begin
      Ident := Strings[I];
      Strings[I] := Ident + Strings.NameValueSeparator + ReadString(Section, Ident, '');
    end;
  finally
    Strings.EndUpdate;
  end;
end;

function TMacPreferencesIniFile.ReadString(const Section, Ident, Default: string): string;
var
  TypeID: CFTypeID;
  Value: CFPropertyListRef;
begin
  Value := ReadValue(Section, Ident);
  if Value = nil then
    Result := Default
  else
    try
      TypeID := CFGetTypeID(Value);
      if TypeID = CFStringGetTypeID then
        Result := CFStringGetValue(Value)
      else if TypeID = CFBooleanGetTypeID then
        Result := BoolToStr(CFBooleanGetValue(Value), True)
      else if TypeID = CFNumberGetTypeID then
        Result := FloatToStr(CFNumberGetAsDouble(Value))
      else if TypeID = CFDateGetTypeID then
        Result := DateTimeToStr(CFDateGetAsTDateTime(Value))
      else
        Result := Default;
    finally
      CFRelease(Value);
    end;
end;

function TMacPreferencesIniFile.ReadValue(const Section, Ident: string): CFPropertyListRef;
var
  Key: CFStringRef;
begin
  if Section <> '' then
    Key := CFStringCreateNoCopy(Section + Delimiter + Ident)
  else
    Key := CFStringCreateNoCopy(Ident);
  try
    Result := CFPreferencesCopyAppValue(Key, FApplicationID);
  finally
    CFRelease(Key);
  end;
end;

procedure TMacPreferencesIniFile.WriteBinaryStream(const Section, Ident: string;
  Stream: TStream);
var
  Data: CFMutableDataRef;
  Length: Integer;
begin
  Length := Integer(Stream.Size - Stream.Position);
  Data := CFDataCreateMutable(nil, Length);
  try
    CFDataSetLength(Data, Length);
    Stream.ReadBuffer(CFDataGetMutableBytePtr(Data)^, Length);
    WriteValue(Section, Ident, Data);
  finally
    CFRelease(Data);
  end;
end;

procedure TMacPreferencesIniFile.WriteBool(const Section, Ident: string; Value: Boolean);
begin
  if Value then
    WriteValue(Section, Ident, kCFBooleanTrue)
  else
    WriteValue(Section, Ident, kCFBooleanFalse);
end;

procedure TMacPreferencesIniFile.WriteDateTime(const Section, Ident: string; Value: TDateTime);
begin
  WriteValue(Section, Ident, CFDateCreateFromTDateTime(Value), True);
end;

procedure TMacPreferencesIniFile.WriteDate(const Section, Ident: string; Value: TDateTime);
begin
  WriteDateTime(Section, Ident, Value);
end;

procedure TMacPreferencesIniFile.WriteFloat(const Section, Ident: string; Value: Double);
begin
  WriteValue(Section, Ident, CFNumberCreate(nil, kCFNumberFloat64Type, @Value), True);
end;

procedure TMacPreferencesIniFile.WriteInteger(const Section, Ident: string; Value: Int32);
begin
  WriteValue(Section, Ident, CFNumberCreate(nil, kCFNumberSInt32Type, @Value), True);
end;

procedure TMacPreferencesIniFile.WriteString(const Section, Ident, Value: string);
begin
  WriteValue(Section, Ident, CFStringCreate(Value), True);
end;

procedure TMacPreferencesIniFile.WriteValue(const Key: string;
  Value: CFPropertyListRef; ReleaseValueAfterWrite: Boolean = False);
var
  S: CFStringRef;
begin
  FAllKeysCache := nil;
  S := CFStringCreate(Key); //*not* CFStringCreateNoCopy since CFPreferencesSetAppValue retains it
  try
    CFPreferencesSetAppValue(S, Value, FApplicationID);
  finally
    CFRelease(S);
    if ReleaseValueAfterWrite and (Value <> nil) then CFRelease(Value);
  end;
end;

procedure TMacPreferencesIniFile.WriteValue(const Section, Ident: string;
  Value: CFPropertyListRef; ReleaseValueAfterWrite: Boolean = False);
var
  Key: string;
begin
  if Section <> '' then
    Key := Section + Delimiter + Ident
  else
    Key := Ident;
  WriteValue(Key, Value, ReleaseValueAfterWrite);
end;

procedure TMacPreferencesIniFile.UpdateFile;
begin
  CFPreferencesAppSynchronize(FApplicationID);
end;

function TMacPreferencesIniFile.ValueExists(const Section, Ident: string): Boolean;
var
  Key, ToFind: string;
begin
  if Section <> '' then
    ToFind := Section + Delimiter + Ident
  else
    ToFind := Ident;
  for Key in GetAllKeys do
    if SameStr(Key, ToFind, loUserLocale) then Exit(True);
  Result := False;
end;

function CreateUserPreferencesIniFile(AWinLocation: TWinLocation): TCustomIniFile;
begin
  Result := TMacPreferencesIniFile.Create;
end;

{$ELSE}

constructor TMacPreferencesIniFile.Create;
begin
  raise ENotSupportedException.Create('TMacPreferencesIniFile only supported on Mac OS X');
end;

function CreateUserPreferencesIniFile(AWinLocation: TWinLocation = TWinLocation.Registry): TCustomIniFile;
var
  CompanyName, ProductName, Path: string;
  {$IFDEF MSWINDOWS}
  Handle, Len: DWORD;
  Data: TBytes;
  CP: PWordArray;
  CharPtr: PChar;
  {$ENDIF}
begin
  Path := GetModuleName(0);
  ProductName := ChangeFileExt(ExtractFileName(Path), '');
  {$IFDEF MSWINDOWS}
  SetLength(Data, GetFileVersionInfoSize(PChar(Path), Handle));
  if GetFileVersionInfo(PChar(Path), Handle, Length(Data), Data) and
     VerQueryValue(Data, 'VarFileInfo\Translation', Pointer(CP), Len) then
  begin
    FmtStr(Path, 'StringFileInfo\%.4x%.4x\', [CP[0], CP[1]]);
    if VerQueryValue(Data, PChar(Path + 'CompanyName'), Pointer(CharPtr), Len) then
      SetString(CompanyName, CharPtr, Len - 1);
    if VerQueryValue(Data, PChar(Path + 'ProductName'), Pointer(CharPtr), Len) then
      SetString(ProductName, CharPtr, Len - 1);
  end;
  {$ENDIF}
  if CompanyName = '' then
    Path := ProductName
  else
    Path := CompanyName + PathDelim + ProductName;
  {$IF DECLARED(TRegistryIniFile)}
  if AWinLocation = TWinLocation.Registry then
  begin
    Result := TRegistryIniFile.Create('Software\' + Path);
    Exit;
  end;
  {$IFEND}
  Path := IncludeTrailingPathDelimiter(GetHomePath) + Path;
  ForceDirectories(Path);
  Result := TMemIniFile.Create(Path + PathDelim + 'Preferences.ini', TEncoding.UTF8);
end;
{$ENDIF}

end.

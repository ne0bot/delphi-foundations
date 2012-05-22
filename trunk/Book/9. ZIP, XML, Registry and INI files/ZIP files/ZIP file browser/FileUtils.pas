unit FileUtils;

interface

uses
  System.SysUtils;

function GetLocalTempDir: string;
function GetRealHomeDir: string;
function ShellOpen(const FileName: string): Boolean;
function TryDOSFileDateToDateTime(FileDate: UInt32; out DateTime: TDateTime): Boolean;

const
  ZipFileExts: array[1..9] of string = ('.zip', '.docx', '.docm', '.pptx', '.pptm',
    '.xlsx', '.xlsm', '.jar', '.xap');

implementation

uses
  System.DateUtils, System.Diagnostics, System.IOUtils,
  {$IFDEF MSWINDOWS}WinApi.Windows, WinApi.ShellApi{$ELSE}Posix.Stdlib{$ENDIF};

function TryDOSFileDateToDateTime(FileDate: UInt32; out DateTime: TDateTime): Boolean;
begin
  Result := TryEncodeDateTime(LongRec(FileDate).Hi shr 9 + 1980,
    LongRec(FileDate).Hi shr 5 and 15, LongRec(FileDate).Hi and 31,
    LongRec(FileDate).Lo shr 11, LongRec(FileDate).Lo shr 5 and 63,
      LongRec(FileDate).Lo and 31 shl 1, 0, DateTime);
end;

function GetLocalTempDir: string;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetTempPath) +
    ChangeFileExt(ExtractFileName(ParamStr(0)), IntToStr(TStopwatch.GetTimeStamp));
end;

function GetRealHomeDir: string;
const
  HomeVarName = {$IFDEF MSWINDOWS}'USERPROFILE'{$ELSE}'HOME'{$ENDIF};
begin
  Result := GetEnvironmentVariable(HomeVarName);
end;

function ShellOpen(const FileName: string): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := ShellExecute(0, nil, PChar(FileName), nil, nil, SW_SHOWNORMAL) > 32;
  {$ELSE}
  Result := _system(PAnsiChar(UTF8String('open "' +
    StringReplace(StringReplace(FileName, '\', '\\', [rfReplaceAll]),
      '"', '\"', [rfReplaceAll]) + '"'))) = 0;
  {$ENDIF};
end;

end.

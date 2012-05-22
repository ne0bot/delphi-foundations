program UnicodeNormalisation;
{
  Demonstrates using the operating system API to both combine decomposed Unicode characters on the
  one hand and break out composable characters on the other.
}
uses
  {$IF Defined(MSWINDOWS)}
  Winapi.Windows, Vcl.Dialogs,
  {$ELSEIF Defined(MACOS)}
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.Foundation, FMX.Dialogs,
  {$ELSE}
  NotImplementedForPlatform,
  {$IFEND}
  System.SysUtils, System.Classes;

{$IFDEF MACOS}
function NSToDelphiString(const NS: NSString): string;
var
  Range: CFRange;
begin
  Range.location := 0;
  Range.length := NS.length;
  SetLength(Result, Range.length);
  CFStringGetCharacters((NS as ILocalObject).GetObjectID, Range, PChar(Result));
end;
{$ENDIF}

function EnsurePrecomposed(const S: string): string;
begin
  if S = '' then Exit('');
{$IF Defined(MSWINDOWS)}
  SetLength(Result, Length(S));
  SetLength(Result, FoldString(MAP_PRECOMPOSED, PChar(S), Length(S),
    PChar(Result), Length(S)));
{$ELSEIF Defined(MACOS)}
  Result := NSToDelphiString(NSSTR(S).precomposedStringWithCanonicalMapping);
{$IFEND}
end;

function EnsureDecomposed(const S: string): string;
begin
  if S = '' then Exit('');
{$IF Defined(MSWINDOWS)}
  SetLength(Result, FoldString(MAP_COMPOSITE, PChar(S), Length(S), nil, 0));
  if FoldString(MAP_COMPOSITE, PChar(S), Length(S), PChar(Result), Length(Result)) = 0 then
    RaiseLastOSError;
{$ELSEIF Defined(MACOS)}
  Result := NSToDelphiString(NSSTR(S).decomposedStringWithCanonicalMapping);
{$IFEND}
end;

const
  PrecomposedStr = 'café';
  DecomposedStr = 'cafe'#$0301;
var
  MsgLines: TStringList;
  S: string;
begin
  MsgLines := TStringList.Create;
  try
    MsgLines.Add('PrecomposedStr = ' + PrecomposedStr +
      ', DecomposedStr = ' + DecomposedStr);
    MsgLines.Add(Format('Length(PrecomposedStr) = %d, Length(DecomposedStr) = %d',
      [Length(PrecomposedStr), Length(DecomposedStr)]));
    MsgLines.Add('');
    MsgLines.Add('PrecomposedStr = DecomposedStr? ' +
      BoolToStr(PrecomposedStr = DecomposedStr, True));
    MsgLines.Add('SameStr(PrecomposedStr, DecomposedStr)? ' +
      BoolToStr(SameStr(PrecomposedStr, DecomposedStr), True));
    MsgLines.Add('SameStr(PrecomposedStr, DecomposedStr, loUserLocale)? ' +
      BoolToStr(SameStr(PrecomposedStr, DecomposedStr, loUserLocale), True));
    MsgLines.Add('');
    S := EnsurePrecomposed(DecomposedStr);
    MsgLines.Add('PrecomposedStr = EnsurePrecomposed(DecomposedStr)? ' +
      BoolToStr(PrecomposedStr = S, True));
    S := EnsureDecomposed(PrecomposedStr);
    MsgLines.Add('EnsureDecomposed(PrecomposedStr) = DecomposedStr? ' +
      BoolToStr(S = DecomposedStr, True));
    ShowMessage(MsgLines.Text);
  finally
    MsgLines.Free;
  end;
end.

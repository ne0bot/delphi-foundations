program StringToStringMapWithTHashedStringList;
{
  Brief demo of the sort of situation where the THashedStringList class can serve as a
  useful 'in the box' alternative to TDictionary when items need to mainted in order,
  together with a simple descendant class that ensures no multiple keys can be added.
  For almost anything else, the need for a sorted dictionary would be better served
  with a dedicated implementation, such as that found in Alexandru Ciobanu's
  'Collections' open source project (http://code.google.com/p/delphi-coll/).
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.RTLConsts,
  System.Classes,
  System.IniFiles,
  System.Generics.Collections;

procedure TestDictionary;
var
  Dict: TDictionary<string, string>;
  Pair: TPair<string, string>;
begin
  WriteLn('*** TDictionary ***');
  WriteLn;
  Dict := TDictionary<string, string>.Create;
  try
    Dict.Add('4th', 'Arsenal');
    Dict.Add('3rd', 'Manchester City');
    Dict.Add('2nd', 'Chelsea');
    Dict.Add('1st', 'Manchester United');
    for Pair in Dict do
      WriteLn(Pair.Key, ': ', Pair.Value);
    WriteLn;
    WriteLn('In third position was ', Dict['3rd']);
  finally
    Dict.Free;
  end;
  WriteLn;
end;

type
  THashedStringListClass = class of THashedStringList;

  TMyHashedStringList = class(THashedStringList)
  strict private
    FAllowValueOverwrite: Boolean;
  protected
    procedure Put(Index: Integer; const S: string); override;
  public
    procedure AddOrSetValue(const AName, AValue: string);
  end;

procedure TMyHashedStringList.AddOrSetValue(const AName, AValue: string);
begin
  FAllowValueOverwrite := True;
  try
    Values[AName] := AValue;
  finally
    FAllowValueOverwrite := False;
  end;
end;

procedure TMyHashedStringList.Put(Index: Integer; const S: string);
begin
  if not FAllowValueOverwrite and (IndexOfName(ExtractName(S)) >= 0) then
    raise EStringListError.CreateRes(@SDuplicateString);
  inherited Put(Index, S);
end;

function CompareKeys(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := CompareStr(List.Names[Index1], List.Names[Index2]);
end;

procedure TestHashedStringList(ListClass: THashedStringListClass);
var
  Dict: THashedStringList;
  I: Integer;
begin
  WriteLn('*** ', ListClass.ClassName, ' ***');
  WriteLn;
  Dict := ListClass.Create;
  try
    Dict.Values['4th'] := 'Arsenal';
    Dict.Values['3rd'] := 'Manchester City';
    Dict.Values['2nd'] := 'Chelsea';
    Dict.Values['1st'] := 'Manchester United';
    for I := 0 to Dict.Count - 1 do
      WriteLn(Dict.Names[I], ': ', Dict.ValueFromIndex[I]);
    WriteLn;
    WriteLn('In third position was ', Dict.Values['3rd']);
    WriteLn;
    Writeln(ListClass.ClassName, ' after sorting');
    Dict.CustomSort(CompareKeys);
    for I := 0 to Dict.Count - 1 do
      WriteLn(Dict.Names[I], ': ', Dict.ValueFromIndex[I]);
    WriteLn;
    WriteLn('Now attempting to add a duplicate entry...');
    Dict.Values['4th'] := 'Liverpool';
    WriteLn('Success!');
  finally
    Dict.Free;
  end;
  WriteLn;
end;

begin
  WriteLn('NB: the TMyHashedStringList test code intentionally causes an exception' +
    sLineBreak + 'at the end by attempting to add a duplicate key.');
  WriteLn('');
  Write('Press ENTER to continue...');
  Readln;
  WriteLn('');
  try
    TestDictionary;
    TestHashedStringList(THashedStringList);
    TestHashedStringList(TMyHashedStringList);
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
  WriteLn('');
  Write('Press ENTER to exit...');
  ReadLn;
end.

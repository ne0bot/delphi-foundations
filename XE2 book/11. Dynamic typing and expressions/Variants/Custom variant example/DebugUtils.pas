unit DebugUtils;
{
  Invokable custom variant example: the Debug variant here exposes one method, Output, that takes an
  arbitrary number of parameters, spittng them all out to the IDE's event log if targeting Windows,
  and the console - typically the Platform Assistant - if targeting OS X.

  Debug.Output('Send me to the event log please!');
  Debug.Output(42, 123.456);

  If using Delphi Starter, the event log won't be available. Use the (free) Sysinternals DebugView
  application, downloadable from Microsoft's website, as a substitute.
}
interface

function Debug: Variant;
procedure OutputDebugText(const S: string);

implementation

uses
  {$IFDEF MSWINDOWS}Winapi.Windows,{$ENDIF} System.SysUtils, System.Classes, System.Variants;

var
  TypeInst: TCustomVariantType;

function Debug: Variant;
begin
  VarClear(Result);
  TVarData(Result).VType := TypeInst.VarType;
end;

procedure OutputDebugText(const S: string);
begin
  {$IFDEF DEBUG}
    {$IFDEF MSWINDOWS}
      OutputDebugString(PChar(S));
    {$ELSE}
      WriteLn('Debug Output: ', S);
    {$ENDIF}
  {$ENDIF}
end;

type
  TDebugVariantType = class(TInvokeableVariantType)
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    function DoProcedure(const V: TVarData; const Name: string;
      const Arguments: TVarDataArray): Boolean; override;
  end;

procedure TDebugVariantType.Clear(var V: TVarData);
begin
  SimplisticClear(V);
end;

procedure TDebugVariantType.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  SimplisticCopy(Dest, Source);
end;

function TDebugVariantType.DoProcedure(const V: TVarData; const Name: string;
  const Arguments: TVarDataArray): Boolean;
var
  I: Integer;
  Strings: TStringList;
begin
  Result := SameText(Name, 'Output') and (Arguments <> nil);
  if not Result then Exit;
  if Length(Arguments) = 1 then
  begin
    OutputDebugText(string(Variant(Arguments[0])));
    Exit;
  end;
  Strings := TStringList.Create;
  try
    for I := Low(Arguments) to High(Arguments) do
      Strings.Add(string(Variant(Arguments[I])));
    Strings.LineBreak := ', ';
    OutputDebugText(Strings.Text);
  finally
    Strings.Free;
  end;
end;

initialization
  TypeInst := TDebugVariantType.Create;
finalization
  TypeInst.Free;
end.

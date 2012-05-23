unit BoundedStrings;
{
  Based on an idea by Andreas Rejbrand (http://stackoverflow.com/a/7588601), though with a
  few refinements of my own. Given its origins, this unit is licenced under Creative 
  Commons Attribution-ShareAlike 3.0 (http://creativecommons.org/licenses/by-sa/3.0/).
}
interface

uses
  System.SysUtils;
  
{$IFDEF DEBUG}
  {$RANGECHECKS ON}
{$ENDIF}
  
type
  TBoundedString = record
  public type
    TLength = 1..High(Integer);
  strict private
    FMinLength, FMaxLength: TLength;
    FValue: string;
    procedure CheckInitialized;
    function GetMinLength: TLength;
    function GetMaxLength: TLength;
    function GetValue: string;
    procedure SetValue(const S: string);
  public
    constructor Create(AMinLength, AMaxLength: TLength); overload;
    constructor Create(AMinLength, AMaxLength: TLength; const AValue: string); overload;
    class operator Add(const A, B: TBoundedString): string; overload;
    class operator Add(const A: TBoundedString; const B: string): string; overload;
    class operator Add(const A: string; const B: TBoundedString): string; overload;
    class operator Implicit(const S: TBoundedString): string;
    class operator Equal(const A, B: TBoundedString): Boolean;
    class operator NotEqual(const A, B: TBoundedString): Boolean;
    property MinLength: TLength read GetMinLength;
    property MaxLength: TLength read GetMaxLength;
    property Value: string read GetValue write SetValue;
  end;
  
implementation
  
resourcestring
  SNotInitialized = 'A TBoundedString instance requiries explicit creation before use';
  SStringTooSmall = 'String too small for the TBoundedString instance';
  SStringTooBig = 'String too big for the TBoundedString instance';

constructor TBoundedString.Create(AMinLength, AMaxLength: TLength);
begin
  FMinLength := AMinLength;
  FMaxLength := AMaxLength;
  FValue := StringOfChar(' ', AMinLength);
end;

constructor TBoundedString.Create(AMinLength, AMaxLength: TLength;
  const AValue: string);
begin
  Create(AMinLength, AMaxLength);
  SetValue(AValue);
end;

procedure TBoundedString.CheckInitialized;
begin
  if FValue = '' then 
    raise EInvalidOpException.CreateRes(@SNotInitialized);
end;

function TBoundedString.GetMinLength: TLength;
begin
  CheckInitialized;
  Result := FMinLength;
end;

function TBoundedString.GetMaxLength: TLength;
begin
  CheckInitialized;
  Result := FMaxLength;
end;

function TBoundedString.GetValue: string;
begin
  CheckInitialized;
  Result := FValue;
end;

procedure TBoundedString.SetValue(const S: string);
begin
  CheckInitialized;
  if Length(S) < FMinLength then 
    raise ERangeError.CreateRes(@SStringTooSmall);
  if Length(S) > FMaxLength then
    raise ERangeError.CreateRes(@SStringTooSmall);
  FValue := S;
end;

class operator TBoundedString.Add(const A, B: TBoundedString): string;
begin
  Result := A.Value + B.Value;
end;

class operator TBoundedString.Add(const A: TBoundedString; const B: string): string;
begin
  Result := A.Value + B;
end;

class operator TBoundedString.Add(const A: string; const B: TBoundedString): string;
begin
  Result := A + B.Value;
end;

class operator TBoundedString.Equal(const A, B: TBoundedString): Boolean;
begin
  Result := A.Value = B.Value;
end;

class operator TBoundedString.NotEqual(const A, B: TBoundedString): Boolean;
begin
  Result := A.Value <> B.Value;
end;

class operator TBoundedString.Implicit(const S: TBoundedString): string;
begin
  Result := S.Value;
end;

end.

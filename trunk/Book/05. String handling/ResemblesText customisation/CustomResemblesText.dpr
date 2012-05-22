program CustomResemblesText;
{
  Trivial example of customising the behaviour of the ResemblesText function.
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.StrUtils;

function MyResemblesTextImpl(const S1, S2: string): Boolean;

  function StripSuffix(const S: string): string;
  begin
    if EndsText('ing', S) then
      Result := Copy(S, 1, Length(S) - 3)
    else
      Result := S;
  end;
begin
  Result := SameText(StripSuffix(S1), StripSuffix(S2), loUserLocale);
end;

procedure Test;
begin
  Write('Does ''shoot'' resemble ''SHOOTING''? ');
  if ResemblesText('shoot', 'SHOOTING') then WriteLn('Yes') else WriteLn('No');
  Write('Does ''shot'' resemble ''shoot''? ');
  if ResemblesText('shot', 'shoot') then WriteLn('Yes') else WriteLn('No');
end;

begin
  WriteLn('Before customisation...');
  Test; //uses the default implementation, i.e. Soundex
  ResemblesProc := MyResemblesTextImpl;
  WriteLn;
  WriteLn('After customisation...');
  Test; //uses the custom implementation
  ReadLn;
end.

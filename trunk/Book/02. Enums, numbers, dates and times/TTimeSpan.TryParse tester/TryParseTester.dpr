program TryParseTester;
{
  Demonstrates the built-in string parsing facility of the TTimeSpan record.
}
{$APPTYPE CONSOLE}

uses
  System.TimeSpan;

var
  S: string;
  TimeSpan: TTimeSpan;
begin
  WriteLn('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~');
  WriteLn('                       TTimeSpan.TryParse Tester');
  WriteLn('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('2.4:22:10.5');
  WriteLn('-0:0:45');
  WriteLn('45');
  WriteLn;
  repeat
    WriteLn('Type the time span string to parse (or nothing to quit) and press ENTER:');
    ReadLn(S);
    if S = '' then Exit;
    WriteLn;
    if not TTimeSpan.TryParse(S, TimeSpan) then
      WriteLn('Not a valid timespan string')
    else
    begin
      WriteLn('Days: ', TimeSpan.Days);
      WriteLn('Hours: ', TimeSpan.Hours);
      WriteLn('Minutes: ', TimeSpan.Minutes);
      WriteLn('Seconds: ', TimeSpan.Seconds);
      WriteLn('Milliseconds: ', TimeSpan.Milliseconds);
      WriteLn('Total ticks: ', TimeSpan.Ticks);
    end;
    WriteLn;
  until False;
end.

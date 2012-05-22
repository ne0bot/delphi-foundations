program ConsoleNumeric;
{
  Demo of three ways to handle the user not inputting numeric input as requested
  in a console application.
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils;

var
  Value: Integer;
  S: string;
begin
  { Method 1: handle EInOutError. }
  Write('Enter an integer: ');
  repeat
    try
      Readln(Value);
      Break;
    except
      on EInOutError do
        Write('Error! Please try again: ')
      else
        raise;
    end;
  until False;
  WriteLn('You entered ', Value);
  WriteLn;

  { Method 2: use IOResult. }
  Write('Enter another integer: ');
  {$I-} //disable raising of EInOutError
  repeat
    Readln(Value);
    if IOResult = 0 then Break;
    Write('Error! Please try again: ');
  until False;
  {$I+} //renable raising of EInOutError
  WriteLn('You entered ', Value);
  WriteLn;


  { Method 3: explicitly convert. }
  Write('Enter another integer: ');
  repeat
    ReadLn(S);
    if TryStrToInt(S, Value) then
      Break
    else
      Write('Error! Please try again: ');
  until False;
  WriteLn('You entered ', Value);
  WriteLn;

  Write('Press ENTER to exit...');
  ReadLn;
end.

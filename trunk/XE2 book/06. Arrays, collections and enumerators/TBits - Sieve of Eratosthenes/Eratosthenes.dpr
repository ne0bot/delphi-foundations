program Eratosthenes;
{
  An implementation of the Sieve of Eratosthenes to calculate prime numbers, using the
  TBits class. The logic works on the basis a value of True means 'has been crossed out'.
}
{$APPTYPE CONSOLE}
{$IOCHECKS OFF}       //Disable ReadLn exceptions and instead check IOResult.
{$OVERFLOWCHECKS OFF} //Explicitly check for integer overflows.

uses
  System.SysUtils,
  System.Classes;

var
  DoneFirst: Boolean;
  MaxNum, I, J: Integer;
  Numbers: TBits;
begin
  repeat
    Write('Type the biggest number to test for ' +
      'and press ENTER (must be at least 2): ');
    ReadLn(MaxNum);
  until (IOResult = 0) and (MaxNum >= 2);
  Numbers := TBits.Create;
  try
    Numbers.Size := MaxNum + 1;            //Add one since TBits indexes from zero.
    for I := 2 to MaxNum do
      if not Numbers[I] then
      begin
        J := I * I;                        //Use the 'start at the square' optimisation.
        while (J <= MaxNum) and (J > 0) do //Second check is for overflow.
        begin
          Numbers[J] := True;
          Inc(J, I);
        end;
      end;
    WriteLn;
    Write('From 1 to ', MaxNum, ', the prime numbers are the following: ');
    DoneFirst := False;
    for I := 2 to MaxNum do
      if not Numbers[I] then
      begin
        if DoneFirst then
          Write(', ', I)
        else
        begin
          DoneFirst := True;
          Write(I);
        end;
      end;
  finally
    Numbers.Free;
  end;
  WriteLn;
  WriteLn;
  Write('Press ENTER to exit...');
  ReadLn;
end.

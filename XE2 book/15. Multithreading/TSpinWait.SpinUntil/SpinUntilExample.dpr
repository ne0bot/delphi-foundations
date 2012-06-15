program SpinUntilExample;
{
  Spinning should only be used on the expectation the waiting time will be very
  short, otherwise blocking primitives (e.g. TEvent) should be used.
}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs;

function CalculateFibonacci(const Num: Integer): Int64;
begin
  if Num <= 1 then
    Result := Num
  else
    Result := CalculateFibonacci(Num - 1) + CalculateFibonacci(Num - 2);
end;

var
  Nth: Integer;
  ExpectedResult: Int64;
  Timeout: LongWord;
  ActualResult: Int64 = High(Int64);
  Thread: TThread;
  PassedTest: Boolean;
begin
  Write('Nth Fibonacci no. to calculate (e.g. 36): ');
  ReadLn(Nth);
  Write('Expected result (e.g. 14930352): ');
  ReadLn(ExpectedResult);
  Write('Timeout in milliseconds (e.g. 200): ');
  ReadLn(Timeout);

  Thread := TThread.CreateAnonymousThread(
    procedure
    begin
      ActualResult := CalculateFibonacci(Nth);
    end);
  Thread.Start;

  PassedTest := TSpinWait.SpinUntil(
                  function : Boolean
                  begin
                    Result := (ActualResult = ExpectedResult)
                  end, Timeout);
  if PassedTest then
    WriteLn('Passed test')
  else
    WriteLn('Failed test');
  Write('Press ENTER to exit...');
  ReadLn;
end.

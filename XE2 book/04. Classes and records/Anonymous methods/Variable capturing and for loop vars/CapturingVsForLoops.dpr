program CapturingVsForLoops;
{
  Variable capturing can catch the unwary!
}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils;

var
  Procs: array[1..5] of TProc;

procedure DoCreateProcs;
var
  I: Integer;
begin
  for I := Low(Procs) to High(Procs) do
    Procs[I] :=
      procedure
      begin
        WriteLn(I);
      end;
end;

procedure DoCreateProcsAlt;

  function CreateProc(I: Integer): TProc;
  begin
    Result :=
      procedure
      begin
        WriteLn(I);
      end;
  end;
var
  I: Integer;
begin
  for I := Low(Procs) to High(Procs) do
    Procs[I] := CreateProc(I);
end;

procedure InvokeProcs;
var
  I: Integer;
begin
  for I := Low(Procs) to High(Procs) do
    Procs[I]();
end;

begin
  WriteLn('Using the naive way, that falls foul of variable capturing...');
  DoCreateProcs;
  InvokeProcs;
  WriteLn('Using a constructor function...');
  DoCreateProcsAlt;
  InvokeProcs;
  ReadLn;
end.

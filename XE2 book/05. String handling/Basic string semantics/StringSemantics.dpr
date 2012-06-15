program StringSemantics;

{$APPTYPE CONSOLE}

uses
  System.SysUtils;

procedure TestStringsAsQuasiValueTypes;
var
  S1, S2: string;
begin
  { Allocate a new string. }
  S1 := StringOfChar('W', 4);
  { Copy on write, so a simple assignment of one string to another only
    copies a reference; casting to Pointer will confirm this. }
  S2 := S1;
  WriteLn('References match: ', Pointer(S1) = Pointer(S2));
  WriteLn('Contents match: ', S1 = S2);
  { Allocate another string, but with identical contents to the first. }
  S2 := StringOfChar('W', 4);
  WriteLn('References match: ', Pointer(S1) = Pointer(S2));
  WriteLn('Contents match: ', S1 = S2);
end;

procedure TestNormalRefTypeBehaviour;
var
  B1, B2: TBytes;
begin
  B1 := BytesOf('Test');
  B2 := BytesOf('Test');
  WriteLn('References match: ', Pointer(B1) = Pointer(B2));
  WriteLn('Contents match: ', B1 = B2);
end;

procedure TestForIn;
var
  C: Char;
  S: string;
begin
  S := 'Hello world';
  for C in S do
    WriteLn(C);
end;

procedure TestIndexing;
var
  I: Integer;
  S: string;
begin
  S := 'hello world';
  for I := 1 to Length(S) do
  begin
    WriteLn(S[I]);
    S[I] := UpCase(S[I]);
  end;
  WriteLn(S);
end;

procedure DoProc(const Name: string; const Proc: TProcedure);
begin
  WriteLn('*** ', Name, ' ***');
  WriteLn;
  Proc;
  WriteLn;
end;

begin
  DoProc('TestStringsAsQuasiValueTypes', TestStringsAsQuasiValueTypes);
  DoProc('TestNormalRefTypeBehaviour', TestNormalRefTypeBehaviour);
  DoProc('TestForIn', TestForIn);
  DoProc('TestIndexing', TestIndexing);
  Write('Press ENTER to exit...');
  ReadLn;
end.

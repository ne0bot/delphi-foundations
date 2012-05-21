library MyUtils;

{$R *.res}

function AddThem(A, B: Int32): Int32; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := A + B;
end;

exports
  AddThem {$IFDEF MACOS}name '_AddThem'{$ENDIF};

begin
end.

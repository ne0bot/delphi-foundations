program PCharVsPointerCasts;
{
  Shows the difference between a string-to-PChar cast and a string-to-Pointer
  one: when the string is empty, the PChar cast returns a pointer to a null
  terminator (#0) where the Pointer cast returns nil. In use, the PChar
  behaviour removes the need for an additional IF check (if Ptr <> nil then ).
}
{$APPTYPE CONSOLE}

var
  S: string;
  Ptr1, Ptr2: PChar;
begin
  S := '';
  Ptr1 := PChar(S);
  Ptr2 := Pointer(S);
  WriteLn('Ptr1 is nil: ', Ptr1 = nil);
  WriteLn('Ptr2 is nil: ', Ptr2 = nil);
  ReadLn;
end.

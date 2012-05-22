program BasicVariantsTest;
{
  Demo of the variant type's not always obvious type coercion behaviour.
}
{$APPTYPE CONSOLE}

uses
  Variants;

var
  V: Variant;
begin
  //will be added as a native Delphi string (UnicodeString) - wouldn't the 'smallest' type be an AnsiString of some sort though...?
  V := 'VBA is the future of programming';
  WriteLn(VarTypeAsText(VarType(V)));
  //VarTypeAsText will report 'String' for backward compat reasons (ahem)
  V := AnsiString('On Error Resume Next forever!');
  WriteLn(VarTypeAsText(VarType(V)));
  //will be added as a Byte
  V := 123;
  WriteLn(VarTypeAsText(VarType(V)));
  //automatically expands the data to an Integer (why not Word?)
  V := V * V;
  WriteLn(VarTypeAsText(VarType(V)));
  //automatically expands to Double?!
  V := V * V * V;
  WriteLn(VarTypeAsText(VarType(V)));
  //same number, but gets put in as a LongWord (reason is that integral constants in Delphi are typed to the smallest type that can fit the number)
  V := 228886641;
  WriteLn(VarTypeAsText(VarType(V)));
  //finally, an Int64 as perhaps originally expected...
  V := Int64(228886641);
  WriteLn(VarTypeAsText(VarType(V)));
  //gets put in as a Currency value
  V := 45.67;
  WriteLn(VarTypeAsText(VarType(V)));
  //er, still a Currency value...
  V := V + 0.00555;
  WriteLn(VarTypeAsText(VarType(V)));
  //now a Double
  V := 45.67555;
  WriteLn(VarTypeAsText(VarType(V)));
  //put in as a boolean (actually a WordBool, despite what VarTypeAsText claims)
  V := True;
  WriteLn(VarTypeAsText(VarType(V)));
  //put in a string that contains a number
  V := '123';
  WriteLn(VarTypeAsText(VarType(V)));
  //add 4 to it - made a Double
  V := V + 4;
  WriteLn(VarTypeAsText(VarType(V)));
  //add '4' to it - still a Double
  V := V + '4';
  WriteLn(VarTypeAsText(VarType(V)));
  //convert to a string and add '4' again - now still a string
  V := VarToStr(V) + '4';
  WriteLn(VarTypeAsText(VarType(V)));
  ReadLn;
end.

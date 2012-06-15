program BoundedStringsDemo;
{
  For a discussion beyond the book, see my earlier blog post:
  http://delphihaven.wordpress.com/2011/12/06/bounded-strings-using-advanced-records/
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  BoundedStrings in 'BoundedStrings.pas';

var
  Max5Str, AtLeast2Max20Str: TBoundedString;
begin
  try
    Max5Str := TBoundedString.Create(1, 5, 'Hello');
    AtLeast2Max20Str := TBoundedString.Create(2, 20,
      Max5Str + ' world');
    WriteLn('Max5Str must have at least ', Max5Str.MinLength,
      ' characters and at most ', Max5Str.MaxLength); 
    WriteLn('Max5Str = ' + Max5Str); 
    WriteLn('AtLeast2Max20Str = ' + AtLeast2Max20Str);
    { Should be OK, because within bounds }
    Max5Str.Value := 'Bye';
    AtLeast2Max20Str.Value := 'cruel world';
    { Test using string concatenation operator (i.e., +) }
    WriteLn(Max5Str + ' ' + AtLeast2Max20Str);
    { Test inequality and equality operators, and sending a
      TBoundedString to a function expecting a normal string }
    if Max5Str <> 'bye' then
      if SameText(Max5Str, 'bye') then
        if Max5Str = 'Bye' then WriteLn('Looking good!'); 
    { Test the actual bounds functionality! }
    WriteLn('Hopefully an exception will now be raised...');
    Max5Str.Value := 'I said goodbye!';
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
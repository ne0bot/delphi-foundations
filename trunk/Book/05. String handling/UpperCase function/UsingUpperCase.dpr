program UsingUpperCase;
{
  Tiny demo to show the need of passing that extra parameter! An alternative is to call
  the AnsiUpperCase function instead, if you don't mind the poor naming.
}
uses
  System.SysUtils, Vcl.Dialogs;

begin
  ShowMessage(UpperCase('café'));
  ShowMessage(UpperCase('café', loUserLocale));
end.

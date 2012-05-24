program CustomVariantExample;
{
  There are such things as 'custom variants', which enable a variant to hold data of your choice
  and/or support property and method calls. This quick demo does the latter - see DebugUtils.pas
  for more info.
}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  DebugUtils in 'DebugUtils.pas';

begin
  Debug.Output('Hello world', 'This is cool', 42);
  ReadLn;
end.

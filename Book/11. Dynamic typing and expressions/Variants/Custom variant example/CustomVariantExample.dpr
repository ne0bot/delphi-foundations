program CustomVariantExample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Variants,
  DebugUtils in 'DebugUtils.pas';

begin
  Debug.Output('Hello world', 'This is cool', 42);
  ReadLn;
end.

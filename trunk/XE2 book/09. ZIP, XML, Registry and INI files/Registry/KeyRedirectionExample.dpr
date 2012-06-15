program KeyRedirectionExample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Win.Registry,
  Winapi.Windows;

const
  Path = '\SOFTWARE\Microsoft\Windows\CurrentVersion';
  ValueName = 'ProgramFilesDir';
var
  Registry: TRegistry;
begin
  WriteLn('Looking under ', Path, ' for ', ValueName, '...');
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    //force reading the 32 bit key
    Registry.Access := KEY_READ or KEY_WOW64_32KEY;
    if not Registry.OpenKey(Path, False) then
      RaiseLastOSError;
    WriteLn('32 bit value: ', Registry.ReadString(ValueName));
    //force reading the 64 bit key and 're-open' it
    Registry.Access := KEY_READ or KEY_WOW64_64KEY;
    if not Registry.OpenKey(Path, False) then
      RaiseLastOSError;
    WriteLn('64 bit value: ', Registry.ReadString(ValueName));
  finally
    Registry.Free;
  end;
  ReadLn;
end.

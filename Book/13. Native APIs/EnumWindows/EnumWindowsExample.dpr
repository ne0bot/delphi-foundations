program EnumWindowsExample;
{
  Simple example of calling the EnumWindows Windows API function, which takes a 
  callback function.
}
{$APPTYPE CONSOLE}

uses
  Winapi.Windows;

function OutputDetails(Wnd: HWND; lParam: LPARAM): BOOL; stdcall;
var
  Buffer: array[0..1023] of Char;
  ClassName, WindowTitle: string;
begin
  SetString(ClassName, Buffer, GetClassName(Wnd, Buffer, Length(Buffer)));
  SetString(WindowTitle, Buffer, GetWindowText(Wnd, Buffer, Length(Buffer)));
  WriteLn(ClassName, ': ', WindowTitle);
  Result := True;
end;

begin
  EnumWindows(@OutputDetails, 0);
  ReadLn;
end.

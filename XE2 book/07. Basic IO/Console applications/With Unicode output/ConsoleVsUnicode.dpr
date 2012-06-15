program ConsoleVsUnicode;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.SysUtils;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  WriteLn('NB: make sure a font like Consolas has been set as the console font, else' + sLineBreak +
    'this won''t work at all. Even then, the third line of Chinese text probably still won''t work.');
  {$ELSE}
  WriteLn('Not targetting Windows, so no special handling. Let''s see the result!');
  {$ENDIF}
  WriteLn;
  WriteLn('Γειά σου Κόσμε');
  WriteLn('Xin chào thế giới');
  WriteLn('您好世界');
  WriteLn;
  Write('Press ENTER to exit...');
  ReadLn;
end.

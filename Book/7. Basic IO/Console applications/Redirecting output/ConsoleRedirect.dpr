program ConsoleRedirect;
{
  Simple demo of redirecting console output to a text file if a given command line
  parameter is set. In the example, if the -uselogfile switch is specified, the
  output is sent to ConsoleRedirect.log. In the IDE, you can test this by alternately
  setting and clearing the switch via the Run|Parameters... command. If it gives you
  grief when trying to clear it, trying setting the parameters to a space.
}
{$APPTYPE CONSOLE}

uses
  {$IFDEF MSWINDOWS}Winapi.Windows, Winapi.ShellApi,{$ENDIF}
  {$IFDEF POSIX}Posix.Stdlib, Posix.Unistd,{$ENDIF}
  System.SysUtils, System.Classes, System.StrUtils;

procedure DoWork;
var
  I: Integer;
begin
  for I := 1 to 100 do
    WriteLn(I);
end;

var
  LogFileName: string;
  LogStream: TFileStream;
begin
  LogStream := nil;
  try
    if FindCmdLineSwitch('uselogfile') then
    begin
      LogFileName := ChangeFileExt(ParamStr(0), '.log');
      LogStream := TFileStream.Create(LogFileName, fmCreate);
      {$IFDEF MSWINDOWS}
      SetStdHandle(STD_OUTPUT_HANDLE, LogStream.Handle);
      {$ENDIF}
      {$IFDEF POSIX}
      dup2(LogStream.Handle, 1);
      {$ENDIF}
    end;
    DoWork;
  finally
    if LogStream = nil then
    begin
      WriteLn('Press ENTER to exit...');
      ReadLn;
    end
    else
    begin
      Flush(Output); //Write/WriteLn do their own bit of buffering
      LogStream.Free;
      {$IFDEF MSWINDOWS}
      ShellExecute(0, nil, 'notepad.exe', PChar(LogFileName), nil, SW_SHOWNORMAL);
      {$ENDIF}
      {$IFDEF POSIX}
      _system(PAnsiChar(UTF8String('open "' +
        ReplaceStr(ReplaceStr(LogFileName, '\', '\\'), '"', '\"') + '"')));
      {$ENDIF}
    end;
  end;
end.

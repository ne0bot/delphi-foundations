program BasicIniFileBehaviour;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.IniFiles,
  System.IOUtils;

const
  SourceData: UTF8String =
    '[MainForm]'                              + sLineBreak +
    'Left=20'                                 + sLineBreak +
    'Top=100'                                 + sLineBreak +
    ''                                        + sLineBreak +
    '[LastFile]'                              + sLineBreak +
    'Path=C:\Users\CCR\Documents\Notes.txt'   + sLineBreak +
    ''                                        + sLineBreak +
    '[MainForm\Toolbars\Standard]'            + sLineBreak +
    'New=1'                                   + sLineBreak +
    'Open=1'                                  + sLineBreak +
    'Save=0'                                  + sLineBreak +
    ''                                        + sLineBreak +
    '[MainForm\Toolbars\Formatting]'          + sLineBreak +
    'Bold=1'                                  + sLineBreak +
    'Italic=0';
var
  IniFileName: string;
  IniFile: TCustomIniFile;
  Stream: TFileStream;
  Strings: TStringList;
  S: string;
begin
  IniFileName := ChangeFileExt(ParamStr(0), '.ini');
  Stream := TFileStream.Create(IniFileName, fmCreate);
  try
    Stream.WriteBuffer(SourceData[1], Length(SourceData));
  finally
    Stream.Free;
  end;
  WriteLn(SourceData);
  WriteLn;
  Strings := nil;
  IniFile := TMemIniFile.Create(IniFileName, TEncoding.UTF8);
  try
    Strings := TStringList.Create;
    IniFile.ReadSections(Strings);
    WriteLn('Sections:');
    WriteLn(Strings.CommaText);
    WriteLn;
    WriteLn('Sub-sections for MainForm:');
    Strings.Clear;
    IniFile.ReadSubSections('MainForm', Strings);
    WriteLn(Strings.CommaText);
    WriteLn;
    WriteLn('Sub-sections for MainForm\Toolbars:');
    Strings.Clear;
    IniFile.ReadSubSections('MainForm\Toolbars', Strings);
    WriteLn(Strings.CommaText);
    WriteLn;
    WriteLn('Sub-sections for MainForm (Recurse = True):');
    Strings.Clear;
    IniFile.ReadSubSections('MainForm', Strings, True);
    WriteLn(Strings.CommaText);
    WriteLn;
    //read value as normal
    WriteLn('ReadInteger(''MainForm'', ''Left'', 0) = ',
      IniFile.ReadInteger('MainForm', 'Left', 0));         //20
    //read a number as text (will work)
    WriteLn('ReadString(''MainForm'', ''Top'', '''') = ',
      IniFile.ReadString('MainForm', 'Top', ''));          //100
    //read a value that doesn't exist
    WriteLn('ReadInteger(''MainForm'', ''Width'', 0) = ',
      IniFile.ReadInteger('MainForm', 'Width', 0));        //0
    ///write that value
    WriteLn('  WriteInteger(''MainForm'', ''Width'', 440)');
    IniFile.WriteInteger('MainForm', 'Width', 440);
    //re-read it (will now succeed)
    WriteLn('ReadInteger(''MainForm'', ''Width'', 0) = ',
      IniFile.ReadInteger('MainForm', 'Width', 0));        //440
    //delete entire section
    WriteLn('  EraseSection(''MainForm'')');
    IniFile.EraseSection('MainForm');
    //re-read key (will return the default value again)
    WriteLn('ReadInteger(''MainForm'', ''Width'', 0) = ',
      IniFile.ReadInteger('MainForm', 'Width', 0));        //0
    //however, 'sub-sections' haven't been removed
    WriteLn('ReadBool(''MainForm\Toolbars\Standard'', ''New'', False) = ',
      IniFile.ReadBool('MainForm\Toolbars\Standard', 'New', False));   //True
  finally
    IniFile.Free;
    Strings.Free;
  end;
  ReadLn;
end.

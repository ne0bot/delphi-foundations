program TStreamReaderIssue;
{
  Demonstrates an issue with TStreamReader - it ignores any default encoding
  you specify if you request it to still check for BOMs and the source stream
  doesn't have one (QC 84071).
}
{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  Vcl.Dialogs;

const
  SourceData: UnicodeString = 'Café Motörhead';
var
  Stream: TMemoryStream;
  Reader: TStreamReader;
begin
  Reader := nil;
  Stream := TMemoryStream.Create;
  try
    Stream.WriteBuffer(SourceData[1],
      Length(SourceData) * StringElementSize(SourceData));
    Stream.Position := 0;
    //try with the actual encoding passed as the default, and BOM auto-detection on...
    Stream.Position := 0;
    Reader := TStreamReader.Create(Stream, TEncoding.Unicode, True);
    ShowMessage('With BOM auto-detection and the actual ' +
      'encoding specified as the default: ' + Reader.ReadLine);
    FreeAndNil(Reader);
    //try with the actual encoding passed as the default, and no BOM auto-detection...
    Stream.Position := 0;
    Reader := TStreamReader.Create(Stream, TEncoding.Unicode);
    ShowMessage('With no BOM auto-detection and the actual ' +
      'encoding specified as the default: ' + Reader.ReadLine);
    FreeAndNil(Reader);
  finally
    Stream.Free;
    Reader.Free;
  end;
end.

unit EnforceMaxZipFileCompression;

interface

implementation

uses
  System.Classes, System.ZLib, System.Zip;

initialization
  TZipFile.RegisterCompressionHandler(zcDeflate,
    function(InStream: TStream; const ZipFile: TZipFile;
      const Item: TZipHeader): TStream
    begin
      Result := TZCompressionStream.Create(clMax, InStream);
    end,
    function(InStream: TStream; const ZipFile: TZipFile;
      const Item: TZipHeader): TStream
    begin
      Result := TZDecompressionStream.Create(InStream);
    end);
end.

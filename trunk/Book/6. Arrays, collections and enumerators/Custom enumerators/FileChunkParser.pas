unit FileChunkParser;

interface

uses
  System.SysUtils, System.Classes;

type
  IChunkParser = interface
    function GetCurrent: TBytes;
    function GetEnumerator: IChunkParser;
    function MoveNext: Boolean;
    property Current: TBytes read GetCurrent;
  end;

  TChunkSize = 1..High(Integer);

function ParseChunks(AStream: TStream; AChunkSize: TChunkSize;
  AOwnsStream: Boolean = False): IChunkParser; overload;
function ParseChunks(const AFileName: string; AChunkSize: TChunkSize): IChunkParser; overload; inline;

implementation

type
  TChunkParser = class(TInterfacedObject, IChunkParser)
  strict protected
    FBuffer: TBytes;
    FOwnsStream: Boolean;
    FStream: TStream;
    function GetCurrent: TBytes;
    function GetEnumerator: IChunkParser;
    function MoveNext: Boolean;
  public
    constructor Create(AStream: TStream; AChunkSize: TChunkSize; AOwnsStream: Boolean);
    destructor Destroy; override;
  end;

{ TChunkParser }

constructor TChunkParser.Create(AStream: TStream; AChunkSize: TChunkSize;
  AOwnsStream: Boolean);
begin
  inherited Create;
  SetLength(FBuffer, AChunkSize);
  FStream := AStream;
  FOwnsStream := AOwnsStream;
end;

destructor TChunkParser.Destroy;
begin
  if FOwnsStream then FreeAndNil(FStream);
  inherited;
end;

function TChunkParser.GetCurrent: TBytes;
begin
  Result := FBuffer;
end;

function TChunkParser.GetEnumerator: IChunkParser;
begin
  Result := Self;
end;

function TChunkParser.MoveNext: Boolean;
var
  BytesRead: Integer;
begin
  Result := False;
  if FStream = nil then Exit;
  BytesRead := FStream.Read(FBuffer[0], Length(FBuffer));
  Result := (BytesRead > 0);
  if BytesRead <> Length(FBuffer) then
  begin
    SetLength(FBuffer, BytesRead);
    if FOwnsStream then FreeAndNil(FStream); //might as well do it now
  end;
end;

{ ParseChunks }

function ParseChunks(AStream: TStream; AChunkSize: TChunkSize;
  AOwnsStream: Boolean = False): IChunkParser;
begin
  Result := TChunkParser.Create(AStream, AChunkSize, AOwnsStream);
end;

function ParseChunks(const AFileName: string; AChunkSize: TChunkSize): IChunkParser;
begin
  Result := ParseChunks(TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite),
    AChunkSize, True)
end;

end.

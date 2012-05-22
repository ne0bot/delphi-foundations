unit BufferedStreams;
{
  Defines proxy classes for buffered stream I/O. ** TBufferedOutputStream needs testing ***
}
interface

uses
  System.SysUtils, System.Classes;

type
  TBufferSize = 16..MaxInt;

  TBufferedStream = class abstract(TStream)
  strict private
    FBaseStream: TStream;
    FOwnsStream: Boolean;
  strict protected
    FBuffer: array of Byte;
    FBufferPos: Integer;
    property BaseStream: TStream read FBaseStream;
    property OwnsStream: Boolean read FOwnsStream;
  public
    constructor Create(ABufferSize: TBufferSize; AStream: TStream;
      AOwnsStream: Boolean = False); overload;
    constructor Create(AStream: TStream; AOwnsStream: Boolean = False); overload;
    destructor Destroy; override;
  end;

  TBufferedInputStream = class(TBufferedStream)
  strict private
    FBufferSize: Integer;
    procedure ReleaseBuffer;
  protected
    function GetSize: Int64; override;
    procedure SetSize(const NewSize: Int64); override;
  public
    destructor Destroy; override;
    function Read(var Buffer; Count: Integer): Integer; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Write(const Buffer; Count: Integer): Integer; override;
  end;

  TBufferedOutputStream = class(TBufferedStream)
  strict protected
    function DoFlush: Boolean;
  protected
    function GetSize: Int64; override;
    procedure SetSize(const NewSize: Int64); override;
  public
    destructor Destroy; override;
    procedure Flush;
    function Read(var Buffer; Count: Integer): Integer; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Write(const Buffer; Count: Integer): Integer; override;
  end;

var
  DefaultBufferSize: TBufferSize = $10000; //64KB

implementation

uses
  System.RTLConsts; //for SWriteError

constructor TBufferedStream.Create(ABufferSize: TBufferSize; AStream: TStream;
  AOwnsStream: Boolean);
begin
  inherited Create;
  FBaseStream := AStream;
  FOwnsStream := AOwnsStream;
  SetLength(FBuffer, ABufferSize);
end;

constructor TBufferedStream.Create(AStream: TStream; AOwnsStream: Boolean = False);
begin
  Create(DefaultBufferSize, AStream, AOwnsStream);
end;

destructor TBufferedStream.Destroy;
begin
  if FOwnsStream then FBaseStream.Free;
  inherited Destroy;
end;

{ TBufferedInputStream }

destructor TBufferedInputStream.Destroy;
begin
  if not OwnsStream then ReleaseBuffer;
  inherited Destroy;
end;

procedure TBufferedInputStream.ReleaseBuffer;
begin
  if (FBufferPos = 0) and (FBufferSize = 0) then Exit;
  BaseStream.Seek(FBufferPos - FBufferSize, soCurrent);
  FBufferPos := 0;
  FBufferSize := 0;
end;

function TBufferedInputStream.GetSize: Int64;
begin
  Result := BaseStream.Size;
end;

procedure TBufferedInputStream.SetSize(const NewSize: Int64);
begin
  ReleaseBuffer;
  BaseStream.Size := NewSize;
end;

function TBufferedInputStream.Read(var Buffer; Count: Integer): Integer;
var
  BytesToReadOffBuffer: Integer;
  SeekPtr: PByte;
begin
  Result := 0;
  SeekPtr := @Buffer;
  while Count > 0 do
  begin
    if FBufferPos = FBufferSize then
    begin
      FBufferPos := 0;
      FBufferSize := BaseStream.Read(FBuffer[0], Length(FBuffer));
      if FBufferSize = 0 then Exit;
    end;
    BytesToReadOffBuffer := FBufferSize - FBufferPos;
    if BytesToReadOffBuffer > Count then BytesToReadOffBuffer := Count;
    Move(FBuffer[FBufferPos], SeekPtr^, BytesToReadOffBuffer);
    Inc(FBufferPos, BytesToReadOffBuffer);
    Inc(SeekPtr, BytesToReadOffBuffer);
    Dec(Count, BytesToReadOffBuffer);
    Inc(Result, BytesToReadOffBuffer);
  end;
end;

function TBufferedInputStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if Origin = soCurrent then
    if (Offset >= 0) and (Offset <= (FBufferSize - FBufferPos)) then
    begin
      Inc(FBufferPos, Offset);
      Result := BaseStream.Seek(0, soCurrent) - FBufferSize + FBufferPos;
      Exit;
    end
    else
      Result := BaseStream.Seek(Offset - FBufferSize + FBufferPos, soCurrent)
  else
    Result := BaseStream.Seek(Offset, Origin);
  FBufferPos := 0;
  FBufferSize := 0;
end;

function TBufferedInputStream.Write(const Buffer; Count: Integer): Integer;
begin
  ReleaseBuffer;
  Result := BaseStream.Write(Buffer, Count);
end;

{ TBufferedOutputStream }

destructor TBufferedOutputStream.Destroy;
begin
  DoFlush;
  inherited Destroy;
end;

function TBufferedOutputStream.DoFlush: Boolean;
var
  BytesWritten: Integer;
begin
  if FBufferPos = 0 then Exit(True); //nothing to flush
  BytesWritten := BaseStream.Write(FBuffer[0], FBufferPos);
  Result := (BytesWritten = FBufferPos);
  if Result then
    FBufferPos := 0
  else
  begin
    Move(FBuffer[BytesWritten], FBuffer[0], FBufferPos - BytesWritten);
    Dec(FBufferPos, BytesWritten);
  end;
end;

procedure TBufferedOutputStream.Flush;
begin
  if not DoFlush then
    raise EWriteError.CreateRes(@SWriteError);
end;

function TBufferedOutputStream.GetSize: Int64;
begin
  Flush;
  Result := BaseStream.Size;
end;

procedure TBufferedOutputStream.SetSize(const NewSize: Int64);
begin
  Flush;
  BaseStream.Size := NewSize;
end;

function TBufferedOutputStream.Read(var Buffer; Count: Integer): Integer;
begin
  Flush;
  Result := BaseStream.Read(Buffer, Count);
end;

function TBufferedOutputStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  //avoid Flush being called if only seeking for the current position
  if (Offset = 0) and (Origin = soCurrent) then
    Result := BaseStream.Seek(0, soCurrent) + FBufferPos
  else
  begin
    Flush;
    Result := BaseStream.Seek(Offset, Origin);
  end;
end;

function TBufferedOutputStream.Write(const Buffer; Count: Integer): Integer;
var
  BytesToBuffer: Integer;
begin
  Result := 0;
  while Count > 0 do
  begin
    if FBufferPos = Length(FBuffer) then
      if not DoFlush and (FBufferPos = Length(FBuffer)) then Exit; //be open to the possibility we could flush some but not all bytes
    BytesToBuffer := Length(FBuffer) - FBufferPos;
    if BytesToBuffer > Count then BytesToBuffer := Count;
    Move(Buffer, FBuffer[FBufferPos], BytesToBuffer);
    Inc(FBufferPos, BytesToBuffer);
    Inc(Result, BytesToBuffer);
    Dec(Count, BytesToBuffer);
  end;
end;

end.

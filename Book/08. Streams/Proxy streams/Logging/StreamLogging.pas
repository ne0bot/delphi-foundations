unit StreamLogging;
{
  A proxy stream class that logs reads, writes and seeks to either a TStrings
  object, a TTextWriter (e.g. TStreamWriter) or file.
}
interface

uses
  System.SysUtils, System.Classes;

type
  TProxyLogStream = class(TStream)
  strict private
    FBaseStream: TStream;
    FLog: TObject;
    FLogType: (ltStrings, ltTextWriter);
    FOwnsLog, FOwnsStream: Boolean;
    procedure WriteToLog(const S: string; const Args: array of const);
  protected
    function GetSize: Int64; override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(ALog: TStrings; ABaseStream: TStream;
      AOwnsBaseStream: Boolean = False); overload;
    constructor Create(ALog: TTextWriter; ABaseStream: TStream;
      AOwnsBaseStream: Boolean = False); overload;
    constructor Create(const ALogFileName: string; AAppend: Boolean;
      ABaseStream: TStream; AOwnsBaseStream: Boolean = False); overload;
    destructor Destroy; override;
    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property BaseStream: TStream read FBaseStream;
  end;

implementation

constructor TProxyLogStream.Create(ALog: TStrings; ABaseStream: TStream;
  AOwnsBaseStream: Boolean = False);
begin
  inherited Create;
  FBaseStream := ABaseStream;
  FOwnsStream := AOwnsBaseStream;
  FLog := ALog;
  FLogType := ltStrings;
end;

constructor TProxyLogStream.Create(ALog: TTextWriter; ABaseStream: TStream;
  AOwnsBaseStream: Boolean);
begin
  inherited Create;
  FBaseStream := ABaseStream;
  FOwnsStream := AOwnsBaseStream;
  FLog := ALog;
  FLogType := ltTextWriter;
end;

constructor TProxyLogStream.Create(const ALogFileName: string; AAppend: Boolean;
  ABaseStream: TStream; AOwnsBaseStream: Boolean);
begin
  FOwnsLog := True;
  Create(TStreamWriter.Create(ALogFileName, AAppend), ABaseStream, AOwnsBaseStream);
end;

destructor TProxyLogStream.Destroy;
begin
  if FOwnsStream then FBaseStream.Free;
  if FOwnsLog then FLog.Free;
  inherited Destroy;
end;

procedure TProxyLogStream.WriteToLog(const S: string; const Args: array of const);
begin
  case FLogType of
    ltStrings: TStrings(FLog).Add(Format(S, Args));
    ltTextWriter: TTextWriter(FLog).WriteLine(S, Args);
  end;
end;

function TProxyLogStream.GetSize: Int64;
begin
  Result := BaseStream.Size;
end;

procedure TProxyLogStream.SetSize(const NewSize: Int64);
begin
  WriteToLog('SetSize: requested to set to %d byte(s)...', [NewSize]);
  BaseStream.Size := NewSize;
  WriteToLog('...completed without incident', []);
end;

function TProxyLogStream.Read(var Buffer; Count: Integer): Integer;
begin
  WriteToLog('Read: requested to read %d byte(s)...', [Count]);
  Result := BaseStream.Read(Buffer, Count);
  WriteToLog('...actually read %d byte(s)', [Result]);
end;

function TProxyLogStream.Write(const Buffer; Count: Integer): Integer;
begin
  WriteToLog('Write: requested to write %d byte(s)...', [Count]);
  Result := BaseStream.Write(Buffer, Count);
  WriteToLog('...actually wrote %d byte(s)', [Result]);
end;

function TProxyLogStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
const
  SOrigin: array[TSeekOrigin] of string = ('beginning', 'current position', 'end');
begin
  if (Offset = 0) and (Origin = soCurrent) then
    WriteToLog('Seek: requested current position...', [])
  else
    WriteToLog('Seek: requested to seek %d byte(s) from %s...', [Offset, SOrigin[Origin]]);
  Result := BaseStream.Seek(Offset, Origin);
  WriteToLog('...returned %d', [Result]);
end;

end.

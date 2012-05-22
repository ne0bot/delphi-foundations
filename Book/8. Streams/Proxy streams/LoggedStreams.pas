unit LoggedStreams;

interface

uses
  System.SysUtils, System.Classes;

type
  TLoggedStream = class(TStream)
  public type
    TLogFileBehaviour = (lfAppend, lfReplace);
  strict private
    FBaseStream: TStream;
    FLog: TStreamWriter;
    FOwnsStream: Boolean;
  protected
    function GetSize: Int64; override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(const ALogFileName: string; ABehaviour: TLogFileBehaviour;
      ABaseStream: TStream; AOwnsBaseStream: Boolean = False);
    destructor Destroy; override;
    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property BaseStream: TStream read FBaseStream;
    property Log: TStreamWriter read FLog;
  end;

implementation

{ TLoggedStream }

constructor TLoggedStream.Create(const ALogFileName: string;
  ABehaviour: TLogFileBehaviour; ABaseStream: TStream; AOwnsBaseStream: Boolean);
begin
  inherited Create;
  FBaseStream := ABaseStream;
  FOwnsStream := AOwnsBaseStream;
  FLog := TStreamWriter.Create(ALogFileName, ABehaviour = lfAppend);
end;

destructor TLoggedStream.Destroy;
begin
  if FOwnsStream then FBaseStream.Free;
  FLog.Free;
  inherited Destroy;
end;

function TLoggedStream.GetSize: Int64;
begin
  Result := BaseStream.Size;
end;

procedure TLoggedStream.SetSize(const NewSize: Int64);
begin
  FLog.WriteLine('SetSize: requested to set to %d bytes', [NewSize]);
  BaseStream.Size := NewSize;
  FLog.WriteLine('         completed without incident');
end;

function TLoggedStream.Read(var Buffer; Count: Integer): Integer;
begin
  FLog.WriteLine('Read: requested to read %d bytes', [Count]);
  Result := BaseStream.Read(Buffer, Count);
  FLog.WriteLine('      actually read %d bytes', [Result]);
end;

function TLoggedStream.Write(const Buffer; Count: Integer): Integer;
begin
  FLog.WriteLine('Write: requested to write %d bytes', [Count]);
  Result := BaseStream.Write(Buffer, Count);
  FLog.WriteLine('       actually wrote %d bytes', [Result]);
end;

function TLoggedStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
const
  SOrigin: array[TSeekOrigin] of string = ('beginning', 'current position', 'end');
begin
  if (Offset = 0) and (Origin = soCurrent) then
    FLog.WriteLine('Seek: requested current position')
  else
    FLog.WriteLine('Seek: requested to seek %d bytes from %s', [Offset, SOrigin[Origin]]);
  Result := BaseStream.Seek(Offset, Origin);
  FLog.WriteLine('      returned %d', [Result]);
end;

end.

unit PipeStream;

interface

uses
  Windows, SysUtils, Classes;

type
  EPipeError = class(EStreamError);

  TPipeStream = class(THandleStream)
  strict private
    FName: string;
  public
    constructor Create(const AName: string; AMode: Word; ABufferSize: DWORD);
    destructor Destroy; override;
    property Name: string read FName;
  end;

implementation

constructor TPipeStream.Create(const AName: string; AMode: Word; ABufferSize: DWORD);
var
  I: Integer;
  PipeName: string;
begin
  FName := AName;
  for I := 1 to Length(FName) do
    if FName[I] = '\' then FName[I] := '/';
  PipeName := '\\.\pipe\' + FName;
  if AMode and fmCreate <> 0 then
    inherited Create(CreateNamedPipe(PChar(PipeName), PIPE_ACCESS_INBOUND, 0,
      PIPE_UNLIMITED_INSTANCES, ABufferSize, ABufferSize, 0, nil))
  else
    inherited Create(FileOpen(PipeName, AMode));
  if Handle = INVALID_HANDLE_VALUE then
    raise EPipeError.Create(SysErrorMessage(GetLastError));
end;

destructor TPipeStream.Destroy;
begin
  if Handle <> 0 then CloseHandle(Handle);
  inherited Destroy;
end;

end.

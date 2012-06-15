unit MREWSyncForPosix;
{
  Version of TMREWSync that delegates to the POSIX API.
}
interface

uses
  System.SysUtils, Posix.SysTypes, Posix.Pthread;

type
  TPosixMREWSync = class(TInterfacedObject, IReadWriteSync)
  strict private
    FHandle: pthread_rwlock_t;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  end;

implementation

constructor TPosixMREWSync.Create;
begin
  inherited Create;
  CheckOSError(pthread_rwlock_init(FHandle, nil));
end;

destructor TPosixMREWSync.Destroy;
begin
  if FHandle <> 0 then pthread_rwlock_destroy(FHandle);
  inherited;
end;

procedure TPosixMREWSync.BeginRead;
begin
  CheckOSError(pthread_rwlock_rdlock(FHandle));
end;

function TPosixMREWSync.BeginWrite: Boolean;
begin
  CheckOSError(pthread_rwlock_wrlock(FHandle));
  Result := False; //can't tell, so prudence says to report intervening writes
end;

procedure TPosixMREWSync.EndRead;
begin
  CheckOSError(pthread_rwlock_unlock(FHandle));
end;

procedure TPosixMREWSync.EndWrite;
begin
  CheckOSError(pthread_rwlock_unlock(FHandle));
end;

end.

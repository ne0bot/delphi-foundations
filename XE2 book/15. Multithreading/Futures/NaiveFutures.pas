unit NaiveFutures;
{
  A naive futures implementation in which each future has its own thread. See BetterFutures.pas for
  a superior implementation that uses its own thread pool instead.
}
interface

uses
  System.SysUtils, System.Classes;

type
  TFuture<T> = record
  strict private type
    TFutureImpl = class(TInterfacedObject, TFunc<T>) //an anonymous method is
    strict private                                   //really an interface with
      FResult: T;                                    //an Invoke method
      FWorker: TThread;
    public
      constructor Create(const ADelegate: TFunc<T>);
      function Invoke: T;
    end;
  public
    class function Create(const ADelegate: TFunc<T>): TFunc<T>; static;
  end;

implementation

{ TFuture<T>.TFutureImpl }

constructor TFuture<T>.TFutureImpl.Create(const ADelegate: TFunc<T>);
begin
  inherited Create;
  FWorker := TThread.CreateAnonymousThread(
    procedure
    begin
      FResult := ADelegate();
    end);
  FWorker.FreeOnTerminate := False;
  FWorker.Start;
end;

function TFuture<T>.TFutureImpl.Invoke: T;
begin
  if FWorker <> nil then
  begin
    FWorker.WaitFor;
    FreeAndNil(FWorker);
  end;
  Result := FResult;
end;

{ TFuture<T> }

class function TFuture<T>.Create(const ADelegate: TFunc<T>): TFunc<T>;
begin
  Result := TFutureImpl.Create(ADelegate);
end;

end.

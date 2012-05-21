unit ProducerConsumerData;
{
  The object is exposed as an interface to simplify memory management.
}
interface

type
  IThreadedData<T> = interface
  ['{4307F0AC-BD82-4842-9C83-D4CB857C6278}']
    function Read: T;                //If there is no data to return, waits until there is.
    procedure Write(const AData: T); //If data previously written has not been read yet, waits until it is.
  end;

type
  TThreadedData<T> = record
  strict private type
    TImplementation = class(TInterfacedObject, IThreadedData<T>)
    strict private
      FData: T;
      FEmpty: Boolean;
    public
      constructor Create;
      function Read: T;
      procedure Write(const AData: T);
    end;
  public
    class function Create: IThreadedData<T>; static; inline;
  end;

implementation

constructor TThreadedData<T>.TImplementation.Create;
begin
  inherited Create;
  FEmpty := True;
end;

function TThreadedData<T>.TImplementation.Read: T;
begin
  TMonitor.Enter(Self); //Since we are exposed only as an interface, Self
  try                   //is private, and therefore, OK to lock against.
    while FEmpty do
      TMonitor.Wait(Self, INFINITE);
    Result := FData;
    FEmpty := True;
    TMonitor.PulseAll(Self);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TThreadedData<T>.TImplementation.Write(const AData: T);
begin
  TMonitor.Enter(Self);
  try
    while not FEmpty do
      TMonitor.Wait(Self, INFINITE);
    FEmpty := False;
    FData := AData;
    TMonitor.PulseAll(Self);
  finally
    TMonitor.Exit(Self);
  end;
end;

class function TThreadedData<T>.Create: IThreadedData<T>;
begin
  Result := TImplementation.Create;
end;

end.

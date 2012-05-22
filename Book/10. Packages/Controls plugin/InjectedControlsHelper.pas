unit InjectedControlsHelper;

interface

uses
  System.Classes, FMX.Types;

type
  TInjectedControl<ControlType: TControl> = record
    class function Create: ControlType; static;
  end;

function InjectedControlsOwner: TComponent;

implementation

var
  FOwner: TComponent = nil;

function InjectedControlsOwner: TComponent;
begin
  Result := FOwner;
end;

class function TInjectedControl<ControlType>.Create: ControlType;
begin
  Result := TComponentClass(ControlType).Create(InjectedControlsOwner) as ControlType;
end;

initialization
  FOwner := TComponent.Create(nil);
finalization
  FOwner.Free;
end.

program ClassConAndDesTest;
{
  Simple test of the order in which class constructors and destructors are
  called in the context of a class hierachy.  
}
uses
  FMX.Dialogs;

type
  TBase = class
    class constructor Create;
    class destructor Destroy;
  end;

  TDescendant = class(TBase)
    class constructor Create;
    class destructor Destroy;
  end;

class constructor TBase.Create;
begin
  ShowMessage('class constructor of TBase');
end;

class destructor TBase.Destroy;
begin
  ShowMessage('class destructor of TBase');
end;

class constructor TDescendant.Create;
begin
  ShowMessage('class constructor of TDescendant');
end;

class destructor TDescendant.Destroy;
begin
  ShowMessage('class destructor of TDescendant');
end;

begin
  TDescendant.ClassName; //actually use it!
end.

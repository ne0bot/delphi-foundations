unit DemoClasses;

interface

uses
  System.SysUtils, System.Classes;

type
  TExampleComponent = class(TComponent)
  end;

  TExpressionUtils = class
  strict private
    function GetNow: TDateTime;
  public
    property Now: TDateTime read GetNow;
  end;

implementation

function TExpressionUtils.GetNow: TDateTime;
begin
  Result := System.SysUtils.Now;
end;

end.

unit ClientForms;
{
  Practically identical to the VCL version - see that for notes.
}
interface

uses
  System.Generics.Collections, FMX.Controls, FMX.Forms;

type
  TClientForm = class;

  TClientFormClass = class of TClientForm;

  TClientForm = class(TForm)
  private class var
    FRegisteredForms: TList<TClientFormClass>;
    class constructor Create;
    class destructor Destroy;
  protected
    class procedure RegisterForm; //class method proper, so we get a Self parameter
  public
    class function Title: string; virtual; abstract;
    class function Description: string; virtual; abstract;
  end;

function RegisteredClientForms: TEnumerable<TClientFormClass>;

implementation

function RegisteredClientForms: TEnumerable<TClientFormClass>;
begin
  Result := TClientForm.FRegisteredForms;
end;

class constructor TClientForm.Create;
begin
  FRegisteredForms := TList<TClientFormClass>.Create;
end;

class destructor TClientForm.Destroy;
begin
  FRegisteredForms.Free;
end;

class procedure TClientForm.RegisterForm;
begin
  if not FRegisteredForms.Contains(Self) then
    FRegisteredForms.Add(Self);
end;

end.

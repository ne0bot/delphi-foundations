unit ClientForms;
{
  Defines both the base class and the factory itself (an ancestor class - TComponent -
  has aleady defined the virtual constructor). Note that the CreateParams override has
  nothing to do with the factory itself - instead, it's to give a descendant form its
  own taskbar entry when shown non-modally.

  WRT to the factory, the pattern for each concrete form is to descend from TClientForm
  (done by manually editing the parent class from TForm), implementing the Title and
  Description methods, before registering in their unit's initialization section. You
  can't register from a class constructor instead since it takes external code using a
  class for the class constructor to get called!

  Note that for completeness sake, the concrete forms have also been removed from the
  application's auto-create list (Project|Options|Forms), their form variables deleted,
  and their uses clauses trimmed. This is just good VCL practice in general though.
}
interface

uses
  System.Generics.Collections, Vcl.Controls, Vcl.Forms;

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
    procedure CreateParams(var Params: TCreateParams); override;
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

procedure TClientForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WndParent := 0;
end;

end.

unit BlueClientForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, ClientForms;

type
  TfrmBlueClient = class(TClientForm)
  public
    class function Title: string; override;
    class function Description: string; override;
  end;

implementation

{$R *.fmx}

class function TfrmBlueClient.Title: string;
begin
  Result := 'Blue form';
end;

class function TfrmBlueClient.Description: string;
begin
  Result := 'A form that''s a Tory until it dies';
end;

initialization
  TfrmBlueClient.RegisterForm;
end.

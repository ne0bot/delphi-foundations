unit RedClientForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, ClientForms;

type
  TfrmRedClient = class(TClientForm)
  public
    class function Title: string; override;
    class function Description: string; override;
  end;

implementation

{$R *.fmx}

class function TfrmRedClient.Title: string;
begin
  Result := 'Red form';
end;

class function TfrmRedClient.Description: string;
begin
  Result := 'A form that''s Labour and proud of it';
end;

initialization
  TfrmRedClient.RegisterForm;
end.

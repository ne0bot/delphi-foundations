unit YellowClientForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, ClientForms;

type
  TfrmYellowClient = class(TClientForm)
  public
    class function Title: string; override;
    class function Description: string; override;
  end;

implementation

{$R *.fmx}

class function TfrmYellowClient.Title: string;
begin
  Result := 'Yellow form';
end;

class function TfrmYellowClient.Description: string;
begin
  Result := 'A form that will ALWAYS agree with Nick';
end;

initialization
  TfrmYellowClient.RegisterForm;
end.

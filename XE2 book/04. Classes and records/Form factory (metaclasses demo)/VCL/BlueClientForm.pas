unit BlueClientForm;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, ClientForms;

type
  TfrmBlueClient = class(TClientForm)
    btnClose: TButton;
    procedure btnCloseClick(Sender: TObject);
  public
    class function Title: string; override;
    class function Description: string; override;
  end;

implementation

{$R *.dfm}

class function TfrmBlueClient.Title: string;
begin
  Result := 'Blue form';
end;

class function TfrmBlueClient.Description: string;
begin
  Result := 'A form that''s a Tory until it dies';
end;

procedure TfrmBlueClient.btnCloseClick(Sender: TObject);
begin
  Close;
end;

initialization
  TfrmBlueClient.RegisterForm;
end.

unit RedClientForm;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, ClientForms;

type
  TfrmRedClient = class(TClientForm)
    btnClose: TButton;
    procedure btnCloseClick(Sender: TObject);
  public
    class function Title: string; override;
    class function Description: string; override;
  end;

implementation

{$R *.dfm}

class function TfrmRedClient.Title: string;
begin
  Result := 'Red form';
end;

class function TfrmRedClient.Description: string;
begin
  Result := 'A form that''s Labour and proud of it';
end;

procedure TfrmRedClient.btnCloseClick(Sender: TObject);
begin
  Close;
end;

initialization
  TfrmRedClient.RegisterForm;
end.

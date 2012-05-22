unit YellowClientForm;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, ClientForms;

type
  TfrmYellowClient = class(TClientForm)
    btnClose: TButton;
    procedure btnCloseClick(Sender: TObject);
  public
    class function Title: string; override;
    class function Description: string; override;
  end;

implementation

{$R *.dfm}

class function TfrmYellowClient.Title: string;
begin
  Result := 'Yellow form';
end;

class function TfrmYellowClient.Description: string;
begin
  Result := 'A form that will ALWAYS agree with Nick';
end;

procedure TfrmYellowClient.btnCloseClick(Sender: TObject);
begin
  Close;
end;

initialization
  TfrmYellowClient.RegisterForm;
end.

unit MainForm;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    btnClone: TButton;
    procedure btnCloneClick(Sender: TObject);
  private
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses ObjectClone;

procedure TForm1.btnCloneClick(Sender: TObject);
var
  NewButton: TButton;
begin
  NewButton := TObjectClone.From(btnClone);
end;

end.

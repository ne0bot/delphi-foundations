unit Btn1Observer;
{
  Form that observes just IButton1ClickedObserver.
}
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, AppEvents;

type
  TfrmBtn1Observer = class(TForm, IButton1ClickedObserver)
    memOutput: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  strict private
    procedure Button1Clicked;
  end;

implementation

{$R *.dfm}

procedure TfrmBtn1Observer.FormCreate(Sender: TObject);
begin
  TAppEvents.RegisterObserver(Self);
end;

procedure TfrmBtn1Observer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmBtn1Observer.Button1Clicked;
begin
  memOutput.Lines.Add('Button 1 was clicked at ' + TimeToStr(Time));
end;

end.

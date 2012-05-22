unit Btn1And2Observer;
{
  Form that observes both IButton1ClickedObserver and IButton2ClickedObserver.
}
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, AppEvents;

type
  TfrmBtns1And2Observer = class(TForm, IButton1ClickedObserver, IButton2ClickedObserver)
    memOutput: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  strict private
    procedure Button1Clicked;
    procedure Button2Clicked;
  end;

implementation

{$R *.dfm}

procedure TfrmBtns1And2Observer.FormCreate(Sender: TObject);
begin
  TAppEvents.RegisterObserver(Self);
end;

procedure TfrmBtns1And2Observer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmBtns1And2Observer.Button1Clicked;
begin
  memOutput.Lines.Add('Button 1 was clicked at ' + TimeToStr(Time));
end;

procedure TfrmBtns1And2Observer.Button2Clicked;
begin
  memOutput.Lines.Add('Button 2 was clicked at ' + TimeToStr(Time));
end;

end.

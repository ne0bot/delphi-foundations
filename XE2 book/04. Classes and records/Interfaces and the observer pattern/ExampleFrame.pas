unit ExampleFrame;
{
  Frame that observes IButton2ClickedObserver.
}
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, AppEvents;

type
  TObserverFrame = class(TFrame, IButton2ClickedObserver)
    Label1: TLabel;
    memOutput: TMemo;
  strict private
    procedure Button2Clicked;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

constructor TObserverFrame.Create(AOwner: TComponent);
begin
  inherited;
  TAppEvents.RegisterObserver(Self);
end;

procedure TObserverFrame.Button2Clicked;
begin
  memOutput.Lines.Add('Button 2 was clicked at ' + TimeToStr(Time));
end;

end.

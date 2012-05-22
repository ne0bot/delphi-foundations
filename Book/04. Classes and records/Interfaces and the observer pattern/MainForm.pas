unit MainForm;
{
  Example of using interfaces to implement the observer pattern, when the observers
  are TComponent descendants (in the present example, TForm or TFrame instances).
  In this implementation, listeners register for any event, and get notified for
  the event interfaces they implement.

  For maintaining a list of registered observers, a TComponentList is used. This
  ensures the list will be automatically updated when any of its items is destroyed
  elsewhere, meaning observers do not have to explicitly unregister themselves.

  When only forms observe, another alternative would be to not have an explicit
  registration system at all, and instead just just the Screen.Forms array property
  instead:

    procedure TfrmMain.Button1Click(Sender: TObject);
    var
      I: Integer;
      Intf: IButton1ClickedObserver;
    begin
      for I := 0 to Screen.FormCount - 1 do
        if Supports(Screen.Forms[I], IButton1ClickedObserver, Intf) then
          Intf.Button1Clicked;
    end;

  This would work in a VCL and FireMonkey application alike. The present example
  is a VCL one just to show a frame amongst the registered obsevers (FireMonkey does
  not have a TFrame implementation in XE2).
}
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Contnrs, AppEvents,
  ExampleFrame;

type
  TfrmMain = class(TForm, IAppEventsSubject)
    btnCreateButton1Observer: TButton;
    btnCreateButton1And2Observer: TButton;
    Button1: TButton;
    Button2: TButton;
    ObserverFrame1: TObserverFrame;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnCreateButton1ObserverClick(Sender: TObject);
    procedure btnCreateButton1And2ObserverClick(Sender: TObject);
  strict private
    FObservers: TComponentList;
    procedure RegisterObserver(Observer: TComponent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses Btn1And2Observer, Btn1Observer;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  FObservers := TComponentList.Create;
  TAppEvents.SetSubject(Self);
  inherited; //called last, since we need to be set up as the event subject ASAP
end;

destructor TfrmMain.Destroy;
begin
  TAppEvents.SetSubject(Self);
  FObservers.Free;
  inherited;
end;

procedure TfrmMain.RegisterObserver(Observer: TComponent);
begin
  if (Observer <> nil) and (FObservers.IndexOf(Observer) < 0) then
    FObservers.Add(Observer);
end;

procedure TfrmMain.btnCreateButton1ObserverClick(Sender: TObject);
var
  Form: TForm;
begin
  Form := TfrmBtn1Observer.Create(Self);
  Form.Show;
end;

procedure TfrmMain.btnCreateButton1And2ObserverClick(Sender: TObject);
var
  Form: TForm;
begin
  Form := TfrmBtns1And2Observer.Create(Self);
  Form.Show;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  I: Integer;
  Intf: IButton1ClickedObserver;
begin
  for I := 0 to FObservers.Count - 1 do
    if Supports(FObservers[I], IButton1ClickedObserver, Intf) then
      Intf.Button1Clicked;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
var
  I: Integer;
  Intf: IButton2ClickedObserver;
begin
  for I := 0 to FObservers.Count - 1 do
    if Supports(FObservers[I], IButton2ClickedObserver, Intf) then
      Intf.Button2Clicked;
end;

end.

unit AppEvents;
{
  Defines the event interfaces. The event generator - the 'subject' - implements IAppEventsSubject
  and registers itself using TAppEvents.SetSubject at startup; listeners then implement one or both
  of IButton1ClickedObserver and IButtonEClickedObserver, and register themsevles by calling
  TAppEvents.RegisterObserver.
}
interface

uses
  System.Classes;

type
  IAppEventsSubject = interface
  ['{0EEBBCCB-968E-4CE9-AC13-AAB4D342269F}']
    procedure RegisterObserver(Observer: TComponent);
  end;

  IButton1ClickedObserver = interface
  ['{7EEE6C3A-D500-4C57-BD6F-27986219E3D8}']
    procedure Button1Clicked;
  end;

  IButton2ClickedObserver = interface
  ['{F83330BF-49B1-4549-9C7E-D279A19D3778}']
    procedure Button2Clicked;
  end;

  TAppEvents = record
  strict private
    class var FSubject: IAppEventsSubject;
  public
    class procedure SetSubject(const ASubject: IAppEventsSubject); static;
    class procedure RegisterObserver(Observer: TComponent); static;
  end;

implementation

class procedure TAppEvents.RegisterObserver(Observer: TComponent);
begin
  FSubject.RegisterObserver(Observer);
end;

class procedure TAppEvents.SetSubject(const ASubject: IAppEventsSubject);
begin
  FSubject := ASubject;
end;

end.

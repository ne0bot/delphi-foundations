unit CCR.Generics.CompList;
{
  Most of the pre-generics list classes have generic equivalents. TComponentList is an
  exception however, so this unit provides the missing generic implementation.
}
interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  TComponentListProxy = class(TComponent) //needs to be declared here to keep the XE compiler happy
  strict private type
    TDeleteNotificationEvent = procedure (Component: TComponent) of object;
  strict private
    FOnDelete: TDeleteNotificationEvent;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(const OnDelete: TDeleteNotificationEvent); reintroduce;
  end;

  TComponentList<T: TComponent> = class(TList<T>)
  strict private
    FOwnsObjects: Boolean;
    FProxy: TComponentListProxy;
    procedure ComponentDeleted(Component: TComponent);
  protected
    procedure Notify(const Item: T; Action: TCollectionNotification); override;
  public
    destructor Destroy; override;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects default False;
  end;

implementation

{ TComponentListProxy }

constructor TComponentListProxy.Create(const OnDelete: TDeleteNotificationEvent);
begin
  inherited Create(nil);
  FOnDelete := OnDelete;
end;

procedure TComponentListProxy.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then FOnDelete(AComponent);
end;

{ TComponentList<T> }

destructor TComponentList<T>.Destroy;
begin
  FreeAndNil(FProxy);
  inherited;
end;

procedure TComponentList<T>.ComponentDeleted(Component: TComponent);
begin
  Remove(Component);
end;

procedure TComponentList<T>.Notify(const Item: T; Action: TCollectionNotification);
begin
  inherited;
  case Action of
    cnAdded:
      begin
        if FProxy = nil then FProxy := TComponentListProxy.Create(ComponentDeleted);
        TComponent(Item).FreeNotification(FProxy);
      end;
    cnRemoved, cnExtracted:
    begin
      if FProxy <> nil then Item.RemoveFreeNotification(FProxy);
      if OwnsObjects and (Action = cnRemoved) then Item.Free;
    end;
  end;
end;

end.

unit Containers;
{
  Defines a simple descendant of TSelection that adds a AllowMovingAndResizing
  property and the ability to be moved with the keyboard.

  Since it is a custom control, it requires explicit registration via
  RegisterFmxClasses in order for ReadComponent to work with it.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Objects;

type
  TContainer = class(TSelection)
  strict private
    FAllowMovingAndResizing: Boolean;
    procedure SetAllowMovingAndResizing(AValue: Boolean);
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure MouseMove(Shift: TShiftState; X: Single; Y: Single); override;
    procedure SetClientSize(const AClientWidth, AClientHeight: Single);
  published
    //new streamable prop
    property AllowMovingAndResizing: Boolean read FAllowMovingAndResizing write SetAllowMovingAndResizing default True;
    //make a few inherited props non-streamable (ideally we wouldn't need to do this!)
    property GripSize stored False;
    property ParentBounds stored False;
    property HideSelection stored False;
  end;

implementation

constructor TContainer.Create(AOwner: TComponent);
begin
  inherited;
  CanFocus := True;
  HideSelection := True;
  Margins.DefaultValue := TRectF.Create(1, 1, 1, 1);
  Margins.Rect := Margins.DefaultValue;
  ParentBounds := False; //allow dragging outside the current client area of the scroll box
  SetAllowMovingAndResizing(True);
end;

procedure TContainer.DoEnter;
begin
  HideSelection := False;
  inherited;
end;

procedure TContainer.DoExit;
begin
  HideSelection := True;
  inherited;
end;

procedure TContainer.KeyDown(var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
var
  Amount: Integer;
begin
  inherited;
  if IsFocused then
  begin
    if ssCtrl in Shift then Amount := 1 else Amount := 4;
    case Key of
      vkLeft: Position.X := Position.X - Amount;
      vkRight: Position.X := Position.X + Amount;
      vkUp: Position.Y := Position.Y - Amount;
      vkDown: Position.Y := Position.Y + Amount;
    end;
  end;
end;

procedure TContainer.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  if AllowMovingAndResizing then inherited;
end;

procedure TContainer.SetAllowMovingAndResizing(AValue: Boolean);
begin
  if AValue = FAllowMovingAndResizing then Exit;
  FAllowMovingAndResizing := AValue;
  if AllowMovingAndResizing then
    GripSize := 4
  else
    GripSize := 1;
end;

procedure TContainer.SetClientSize(const AClientWidth, AClientHeight: Single);
begin
  SetBounds(Position.X, Position.Y, Margins.Left + AClientWidth + Margins.Right,
    Margins.Top + AClientHeight + Margins.Bottom);
end;

initialization
  RegisterFmxClasses([TContainer]);
end.

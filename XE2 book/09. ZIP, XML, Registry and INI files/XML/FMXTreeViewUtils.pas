unit FMXTreeViewUtils;

interface

uses
  System.Classes, System.UITypes, FMX.TreeView;

function HandleTreeViewKeyDown(TreeView: TCustomTreeView; var Key: Word;
  var KeyChar: Char; Shift: TShiftState): Boolean;
function HandleTreeviewMouseDown(TreeView: TCustomTreeView;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single): Boolean;

implementation

function HandleTreeViewKeyDown(TreeView: TCustomTreeView; var Key: Word;
  var KeyChar: Char; Shift: TShiftState): Boolean;

  procedure ChangeIsExpanded(MakeExpanded: Boolean);
  var
    Item: TTreeViewItem;
  begin
    Item := TreeView.Selected;
    if Item = nil then Exit;
    if MakeExpanded then
      if Item.IsExpanded then
      begin
        if Item.Count > 0 then TreeView.Selected := Item[0];
      end
      else
        Item.IsExpanded := True
    else if Item.IsExpanded then
      Item.IsExpanded := False
    else if Item.ParentItem <> nil then
      TreeView.Selected := Item.ParentItem;
    Key := 0;
    Result := True;
  end;
begin
  Result := False;
  case Key of
    vkLeft: ChangeIsExpanded(False);
    vkRight: ChangeIsExpanded(True);
  end;
end;

function HandleTreeviewMouseDown(TreeView: TCustomTreeView;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single): Boolean;
var
  Item: TTreeViewItem;
begin
  Result := False;
  if ssDouble in Shift then
  begin
    Item := TreeView.ItemByPoint(X, Y);
    if Item <> nil then
    begin
      Item.IsExpanded := not Item.IsExpanded;
      Result := True;
    end;
  end;
end;

end.

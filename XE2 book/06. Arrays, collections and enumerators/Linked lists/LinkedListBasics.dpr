program LinkedListBasics;
{
  Example of a doubly-linked list.
}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils;

type
  TNode = class
    Prev, Next: TNode;
    Data: string;
  end;

var
  LastNode: TNode;

function AddNode(const AData: string): TNode;
begin
  Result := TNode.Create;
  Result.Data := AData;
  Result.Prev := LastNode;
  if LastNode <> nil then LastNode.Next := Result;
  LastNode := Result;
end;

procedure DeleteNode(ANode: TNode);
begin
  if ANode.Prev <> nil then ANode.Prev.Next := ANode.Next;
  if ANode.Next <> nil then ANode.Next.Prev := ANode.Prev;
  if LastNode = ANode then LastNode := ANode.Prev;
  ANode.Free;
end;

procedure ClearNodes;
begin
  while LastNode <> nil do DeleteNode(LastNode);
end;

var
  TopNode, Node: TNode;
begin
  TopNode := AddNode('First');
  AddNode('Second');
  AddNode('Third');
  //enumerate
  Node := TopNode;
  repeat
    WriteLn(Node.Data);
    Node := Node.Next;
  until Node = nil;
  //delete a node and enumerate again to check we have the DeleteNode logic right
  DeleteNode(TopNode.Next);
  Node := TopNode;
  repeat
    WriteLn(Node.Data);
    Node := Node.Next;
  until Node = nil;
  //clean up
  ClearNodes;
  ReadLn;
end.

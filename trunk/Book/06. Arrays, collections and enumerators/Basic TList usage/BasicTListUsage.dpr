program BasicTListUsage;

{$APPTYPE CONSOLE}

uses
  System.Generics.Collections, System.Generics.Defaults;

var
  I: Integer;
  List: TList<Integer>; //define a list of integers
  AnotherList: TList<Integer>;
begin
  List := TList<Integer>.Create;
  try
    //add an individual item
    List.Add(99);
    //add a collection - here, open array - of items
    List.AddRange([45, 83, 13]);
    //enumerate what we have
    WriteLn('Enumerating...');
    for I in List do
      WriteLn(' ', I);
    //delete the second item (TList is zero indexed)
    List.Delete(1);
    //insert an item at the top of the list
    List.Insert(0, 361);
    //create another list, add some items to it, then copy them over
    AnotherList := TList<Integer>.Create;
    try
      AnotherList.AddRange([1, 2, 3, 4, 5]);
      List.AddRange(AnotherList);
    finally
      AnotherList.Free;
    end;
    //move the first item to the end, before switching the position of a couple of items
    List.Move(0, List.Count - 1);
    List.Exchange(2, 3);
    //enumerate again, this time via indexing
    WriteLn('Enumerating...');
    for I := 0 to List.Count - 1 do
      WriteLn(' ', List[I]);
    //look for a specific item - IndexOf doesn't require sorting first
    Writeln('Index of 99 = ', List.IndexOf(99));
    //sort and enumerate once more
    List.Sort;
    WriteLn('Enumerating...');
    for I in List do
      WriteLn(' ', I);
    //search for an item
    if List.BinarySearch(99, I) then
      WriteLn('99 is now at position ', I)
    else
      WriteLn('Couldn''t find 99 in the list', I);
    //sort back to front
    List.Sort(TComparer<Integer>.Construct(
      function (const L, R: Integer): Integer
      begin
        Result := R - L;
      end));
    WriteLn('Enumerating...');
    for I in List do
      WriteLn(' ', I);
    //clear the list and prove the point by outputting the new count
    List.Clear;
    Writeln('New count = ', List.Count);
  finally
    List.Free;
  end;
  WriteLn;
  Write('Press ENTER to exit...');
  ReadLn;
end.

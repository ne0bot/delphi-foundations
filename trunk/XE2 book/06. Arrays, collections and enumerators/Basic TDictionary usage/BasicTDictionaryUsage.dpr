program BasicTDictionaryUsage;

{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.Generics.Collections, System.Generics.Defaults;

procedure Foo;
begin
  WriteLn('Foo says hello');
end;

procedure Bar;
begin
  WriteLn('Bar says hello');
end;

function PointerToStr(const P: Pointer): string; inline;
begin
  FmtStr(Result, '$%p', [P]);
end;

var
  Dict: TDictionary<string, TProcedure>;
  Name: string;
  Proc: TProcedure;
  Item: TPair<string, TProcedure>;
begin
  //create the dictionary with the stock case insensitive equality comparer
  Dict := TDictionary<string, TProcedure>.Create(TIStringComparer.Ordinal);
  try
    //add a couple of items, the second with the same Proc as the first
    Dict.Add('Foo', Foo);
    Dict.Add('Bar', Foo);
    //change the item keyed to 'Bar'
    Dict['Bar'] := Bar;
    //check for a given Name, followed by a given Proc
    if Dict.ContainsKey('Foo') then
      WriteLn('"Foo" exists as a key');
    if Dict.ContainsValue(Bar) then
      WriteLn('The "Bar" procedure has been added as a value');
    //enumerate the keys
    WriteLn('Keys:');
    for Name in Dict.Keys do
      WriteLn('  ', Name);
    //enumerate the values
    WriteLn('Values expressed as procedural addresses:');
    for Proc in Dict.Values do
      WriteLn(Format('  $%p', [@Proc]));
    //enumerate the keys-and-values, expressed as TPair instances, executing the procs in turn
    WriteLn('Key/value pairs:');
    for Item in Dict do
    begin
      Write('  Calling ', Item.Key, '... ');
      Item.Value;
    end;
    //as for a procedure to call
    Write('Enter the name of the procedure to call and press ENTER: ');
    ReadLn(Name);
    if Dict.TryGetValue(Name, Proc) then
      Proc
    else
      WriteLn('Could not find "', Name, '"');
  finally
    Dict.Free;
  end;
  WriteLn;
  Write('Press ENTER to exit...');
  ReadLn;
end.

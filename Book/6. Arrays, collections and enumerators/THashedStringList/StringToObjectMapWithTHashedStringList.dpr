program StringToObjectMapWithTHashedStringList;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.IniFiles;

type
  TTeamDetails = class
    Driver1, Driver2, EngineSupplier: string;
    constructor Create(const ADriver1, ADriver2, AEngineSupplier: string);
  end;

constructor TTeamDetails.Create(const ADriver1, ADriver2,
  AEngineSupplier: string);
begin
  inherited Create;
  Driver1 := ADriver1;
  Driver2 := ADriver2;
  EngineSupplier := AEngineSupplier;
end;

var
  Details: TTeamDetails;
  I: Integer;
  Map: THashedStringList;
begin
  Map := THashedStringList.Create;
  try
    Map.OwnsObjects := True;
    //load the items
    Map.AddObject('McLaren', TTeamDetails.Create('Lewis Hamilton',
      'Jenson Button', 'Mercedes'));
    Map.AddObject('Red Bull', TTeamDetails.Create('Sebastian Vettel',
      'Mark Webber', 'Renault'));
    Map.AddObject('Ferrari', TTeamDetails.Create('Fernando Alonso',
      'Felipe Massa', 'Ferrari'));
    //enumerate
    for I := 0 to Map.Count - 1 do
    begin
      Details := Map.Objects[I] as TTeamDetails;
      WriteLn(Map[I] + ': ', Details.Driver1, ' and ', Details.Driver2);
    end;
    //test whether items exists
    if Map.IndexOf('Virgin') < 0 then
      WriteLn('The Virgin team does not exist');
    //get an item by its 'key'
    Details := Map.Objects[Map.IndexOf('Mclaren')] as TTeamDetails;
    WriteLn('McLaren use a ', Details.EngineSupplier, ' engine');
    //sort the 'keys' and enumerate again
    Map.Sort;
    Write('Now sorted:');
    for I := 0 to Map.Count - 1 do
      Write(' ' + Map[I]);
  finally
    Map.Free;
  end;
  ReadLn;
end.

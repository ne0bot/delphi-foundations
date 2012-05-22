program ObjectCloning;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  ObjectClone in 'ObjectClone.pas';

type
  TPersonKind = (pkNone, pkNormal, pkSpecial);
  TPersonKinds = set of TPersonKind;

  TPerson = class // implicit inheritance from TObject
  private
    FName: string;
    FAge: Integer;
    FPersonKinds: TPersonKinds;
  public
    function ToString: string; override;
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
    property PersonKinds: TPersonKinds read FPersonKinds write FPersonKinds;
  end;

function TPerson.ToString: string;
begin
  Result := Format('Name: %s, Age: %d', [Name, Age]);
end;

var
  Source, Dest: TPerson;
begin
  Dest := nil;
  Source := TPerson.Create;
  try
    Source.Age := 42;
    Source.Name := 'Bob';
    Dest := TObjectClone.From(Source);
    Writeln(Dest.ToString);
  finally
    Source.Free;
    Dest.Free;
  end;
  Readln;
end.

unit PluginManager;
{
  Before a package is unloaded, any objects from it need to be freed first. This
  unit helps ensure this when those objects are TComponent descendants.
}
interface

uses
  System.SysUtils, System.Classes;

function LoadPlugin(const AFileName: string): HMODULE;
procedure RegisterPluginComponent(APluginHandle: HMODULE; AObject: TComponent);
procedure UnloadPlugin(AHandle: HMODULE);
procedure UnloadAllPlugins;

function TryGetPluginDescription(const AFileName: string; var ADescription: string): Boolean;

implementation

uses
  System.Contnrs, System.Generics.Collections;

var
  Dictionary: TDictionary<HMODULE,TComponentList>;

function LoadPlugin(const AFileName: string): HMODULE;
begin
  Result := LoadPackage(AFileName);
  Dictionary.Add(Result, TComponentList.Create(True));
end;

procedure RegisterPluginComponent(APluginHandle: HMODULE; AObject: TComponent);
begin
  Dictionary[APluginHandle].Add(AObject);
end;

procedure UnloadPlugin(AHandle: HMODULE);
begin
  Dictionary[AHandle].Free;
  Dictionary.Remove(AHandle);
  UnloadPackage(AHandle);
end;

procedure UnloadAllPlugins;
var
  Pair: TPair<HMODULE,TComponentList>;
begin
  for Pair in Dictionary.ToArray do
  begin
    Pair.Value.Free;
    UnloadPackage(Pair.Key);
    Dictionary.Remove(Pair.Key);
  end;
end;

function TryGetPluginDescription(const AFileName: string; var ADescription: string): Boolean;
begin
  Result := True;
  try
    ADescription := GetPackageDescription(PChar(AFileName));
  except
    on EPackageError do
      Result := False;
  end;
end;

initialization
  Dictionary := TDictionary<HMODULE,TComponentList>.Create;
finalization
  UnloadAllPlugins;
  Dictionary.Free;
end.

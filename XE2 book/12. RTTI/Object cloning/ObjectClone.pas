unit ObjectClone;

{$IFDEF MSWINDOWS}
  {$DEFINE HandleVCLParent}
{$ENDIF}

interface

uses
  System.SysUtils, System.Classes, System.TypInfo, System.RTTI;

type
  TObjectClone = record
  strict private
    type
      TPersistentAccess = class(TPersistent);
    class var FDefaultAssignAddr, FDefaultAssignToAddr: Pointer;
    class constructor Create;
    class function CheckForPersistent<T: class>(AType: TRttiInstanceType; Source, Dest: T): Boolean; static;
    class function CheckForList<T: class>(AType: TRttiType; Source, Dest: T): Boolean; static;
  public
    class function From<T: class>(Source: T): T; static;
  end;

implementation

{$IFDEF HandleVCLParent}
uses Vcl.Controls;
{$ENDIF}

class constructor TObjectClone.Create;
var
  Method: procedure (AObject: TPersistent) of object;
  Temp: TPersistentAccess;
begin
  Temp := TPersistentAccess.Create;
  try
    Method := Temp.Assign;
    FDefaultAssignAddr := TMethod(Method).Code;
    Method := Temp.AssignTo;
    FDefaultAssignToAddr := TMethod(Method).Code;
  finally
    Temp.Free;
  end;
end;

class function TObjectClone.CheckForPersistent<T>(AType: TRttiInstanceType; Source, Dest: T): Boolean;
var
  Method: procedure (AObject: TPersistent) of object;
begin
  //check for a TPersistent descendant that implements Assign or AssignTo
  Result := False;
  if not AType.MetaclassType.InheritsFrom(TPersistent) then Exit;
  Method := TPersistent(Dest).Assign;
  if TMethod(Method).Code <> FDefaultAssignAddr then
  begin
    TPersistent(Dest).Assign(TPersistent(Source));
    Exit(True);
  end;
  Method := TPersistentAccess(Source).AssignTo;
  if TMethod(Method).Code <> FDefaultAssignToAddr then
  begin
    TPersistentAccess(Source).AssignTo(TPersistent(Dest));
    Exit(True);
  end;
end;

class function TObjectClone.CheckForList<T>(AType: TRttiType; Source, Dest: T): Boolean;
var
  Method: TRttiMethod;
  Params: TArray<TRttiParameter>;
begin
  //check for a stock generic TList
  for Method in AType.GetMethods('AddRange') do
    if Method.HasExtendedInfo then
    begin
      Params := Method.GetParameters;
      if (Length(Params) = 1) and (Params[0].ParamType.TypeKind = tkClass) then
      begin
        Method.Invoke(Dest, [Source]);
        Exit(True);
      end;
    end;
  Result := False;
end;

class function TObjectClone.From<T>(Source: T): T;
var
  Context: TRttiContext;
  IsComponent, LookOutForNameProp: Boolean;
  RttiType: TRttiInstanceType;
  Method: TRttiMethod;
  MinVisibility: TMemberVisibility;
  Params: TArray<TRttiParameter>;
  Prop: TRttiProperty;
  IndexedProp: TRttiIndexedProperty;
  Indices: array[0..0] of TValue;
  Count, I: Integer;
begin
  RttiType := Context.GetType(Source.ClassType).AsInstance;
  //find a suitable constructor, though treat components specially
  IsComponent := (Source is TComponent);
  for Method in RttiType.GetMethods do
    if Method.IsConstructor then
    begin
      Params := Method.GetParameters;
      if Params = nil then Break;
      if (Length(Params) = 1) and IsComponent and
         (Params[0].ParamType is TRttiInstanceType) and
         SameText(Method.Name, 'Create') then Break;
    end;
  if Params = nil then
    Result := Method.Invoke(Source.ClassType, []).AsType<T>
  else
    Result := Method.Invoke(Source.ClassType, [TComponent(Source).Owner]).AsType<T>;
  try
    //is it a TPersistent descendant that implements either Assign or AssignTo?
    if CheckForPersistent(RttiType, Source, Result) then Exit;
{$IFDEF HandleVCLParent}
    //many VCL control properties require the Parent property to be set first
    if Source is TControl then TControl(Result).Parent := TControl(Source).Parent;
{$ENDIF}
    //loop through the props, copying values across for ones that are read/write
    LookOutForNameProp := IsComponent and (TComponent(Source).Owner <> nil);
    if IsComponent then
      MinVisibility := mvPublished //an alternative is to build an exception list...
    else
      MinVisibility := mvPublic;
    for Prop in RttiType.GetProperties do
      if (Prop.Visibility >= MinVisibility) and Prop.IsReadable then
        if Prop.IsWritable then
          if LookOutForNameProp and (Prop.Name = 'Name') and
            (Prop.PropertyType is TRttiStringType) then
            LookOutForNameProp := False
          else
            Prop.SetValue(TObject(Result), Prop.GetValue(TObject(Source)));
    CheckForList<T>(RttiType, Source, Result);
  except
    Result.Free;
    raise;
  end;
end;

end.

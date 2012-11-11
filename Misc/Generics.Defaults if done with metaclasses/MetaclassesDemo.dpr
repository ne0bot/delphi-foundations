program MetaclassesDemo;

{$APPTYPE CONSOLE}

uses
  Types,
  SysUtils,
  MetaclassGenerics in 'MetaclassGenerics.pas';

type
  TExampleStateComparer<TypeToAttachState> = class(TStringComparer)
    class var IgnoreCase: Boolean;
    class var LocaleOptions: TLocaleOptions;
    class function Compare(const Left, Right: string): Integer; override;
  end;

class function TExampleStateComparer<TypeToAttachState>.Compare(const Left, Right: string): Integer;
begin
  if IgnoreCase then
    Result := CompareText(Left, Right, LocaleOptions)
  else
    Result := CompareStr(Left, Right, LocaleOptions)
end;

procedure TestStateComparer;
var
  List: TList<string>;
begin
  List := TList<string>.Create(TExampleStateComparer<Byte>);
  try

  finally
    List.Free;
  end;
end;

var
  IntList: TList<Integer>;
  EnumList: TList<TNameType>;
  StringList: TList<string>;
  RectList: TList<TRect>;
begin
  ReportMemoryLeaksOnShutdown := True;
  //this should end up outputting four 1s.
  try
    IntList := TList<Integer>.Create;
    try
      IntList.Add(42);
      IntList.Add(999);
      IntList.Add(123);
      Writeln(IntList.IndexOf(999));
    finally
      IntList.Free;
    end;
    EnumList := TList<TNameType>.Create;
    try
      EnumList.Add(ntDcpBpiName);
      EnumList.Add(ntRequiresPackage);
      EnumList.Add(ntContainsUnit);
      Writeln(EnumList.IndexOf(ntRequiresPackage));
    finally
      EnumList.Free;
    end;
    StringList := TList<string>.Create;
    try
      StringList.Add('hello');
      StringList.Add('metaclass');
      StringList.Add('world');
      Writeln(StringList.IndexOf('metaclass'));
    finally
      StringList.Free;
    end;
    RectList := TList<TRect>.Create;
    try
      RectList.Add(Rect(1, 2, 3, 44));
      RectList.Add(Rect(1, 2, 3, 4));
      RectList.Add(Rect(100, 200, 656, 2340));
      Writeln(RectList.IndexOf(Rect(1, 2, 3, 4)));
    finally
      RectList.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.

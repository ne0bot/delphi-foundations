program AttrTest;

{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.Classes, System.TypInfo, System.Rtti;
  
type
  AppraisalAttribute = class(TCustomAttribute)
  strict private
    FComment, FName: string;
  public
    constructor Create(const AComment, AName: string);
    property Comment: string read FComment;
    property Name: string read FName;
  end;

constructor AppraisalAttribute.Create(const AComment, AName: string);
begin
  FComment := AComment;
  FName := AName;
end;

procedure WriteAppraisalInfo(AClass: TClass);
var
  Context: TRttiContext;
  LType: TRttiType;
  Attrib: TCustomAttribute;
  Appraisal: AppraisalAttribute;
  Method: TRttiMethod;
begin
  LType := Context.GetType(AClass);
  if LType = nil then Exit;
  for Attrib in LType.GetAttributes do
    if Attrib is AppraisalAttribute then
    begin
      Appraisal := AppraisalAttribute(Attrib);
      WriteLn(AClass.ClassName, ': ',
        Appraisal.Comment, ' [', Appraisal.Name, ']');
      Break;
    end;
  for Method in LType.GetMethods do
    for Attrib in Method.GetAttributes do
      if Attrib is AppraisalAttribute then
      begin
        Appraisal := AppraisalAttribute(Attrib);
        WriteLn('  Method ', Method.Name, ': ',
          Appraisal.Comment, ' [', Appraisal.Name, ']');
        Break;
      end;
end;

type
  [Appraisal('This class needs to be more fleshed out', 'Joe Bloggs')]
  TExample = class
    [Appraisal('Well tested', 'John Smith')]
    procedure Foo; virtual; abstract;
  end;
  
begin
  WriteAppraisalInfo(TExample);
end.
program MethodRefAssignments;
{
  Quick illustration of how a method reference can be assigned an anonymous method,
  ordinary method or standalone routine.
}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils;

type
  TMyStandaloneFunc = function (const S: string): Boolean;
  TMyAnonMethod = reference to function (const S: string): Boolean;

function Standalone(const S: string): Boolean;
begin
  Result := (S = 'I stand alone');
end;

type
  TMyObj = class
    function Method(const S: string): Boolean;
  end;

function TMyObj.Method(const S: string): Boolean;
begin
  Result := (S = 'I am part of an object');
end;

var
  SimplePtr: TMyStandaloneFunc;
  Func1, Func2, Func3: TMyAnonMethod;
  Obj: TMyObj;
begin
  //assign to a standalone function
  Func1 := Standalone;
  //assign to a method
  Obj := TMyObj.Create;
  Func2 := Obj.Method;
  //assign to an anonymous method
  Func3 :=
    function (const S: string): Boolean
    begin
      Result := (S = 'I am embedded in foreign territory');
    end;
  //assign simple procedural pointer and do a some equality testing
  SimplePtr := Standalone;
  WriteLn('SimplePtr assigned to Standalone? ', (@SimplePtr = @Standalone));
  WriteLn('Func1 assigned to Standalone? ', (@Func1 = @Standalone));
  ReadLn;
end.

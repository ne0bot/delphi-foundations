library DelphiInterfaceTest;
{
  Example of COM-based Delphi->C# interop that doesn't actually use 'full fat'
  COM, just the respect language's built-in support for basic COM functionality
  (the IUnknown and BSTR types, together with mapping native exceptions to COM
  error codes and vice versa).

  Both the Delphi and C# projects have been set up to output to the same Bin
  folder - you will need to have built the Delphi DLL in the appropriate build
  configuration (i.e., debug or release) before running the C# host.
}
type
  ICalculator = interface
  ['{B1909D19-8DF8-4A4F-AF71-D0EFC653912F}']
    function AddThem(Num1, Num2: Int32): Int32; safecall;
    function Description: WideString; safecall;
  end;

  TCalculator = class(TInterfacedObject, ICalculator)
    function AddThem(Num1, Num2: Int32): Int32; safecall;
    function Description: WideString; safecall;
  end;

function TCalculator.AddThem(Num1, Num2: Int32): Int32;
begin
  Result := Num1 + Num2;
end;

function CreateCalculator: ICalculator; safecall;
begin
  Result := TCalculator.Create;
end;

function TCalculator.Description: WideString;
begin
  Result := ClassName;
end;

exports
  CreateCalculator;
end.

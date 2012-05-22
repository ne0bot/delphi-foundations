program ExprEngineBasics;
{
  Demonstrates the expression engine at its most basic, used to evaluate the
  expression '1 + 1'.
}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Rtti, System.Bindings.EvalProtocol,
  System.Bindings.EvalSys, System.Bindings.Evaluator;

var
  Scope: IScope;
  CompiledExpr: ICompiledBinding;
  Result: TValue;
begin
  { Use the stock implementation of things like the addition operator }
  Scope := BasicOperators;
  { Compile our expression }
  CompiledExpr := Compile('1 + 1', Scope);
  { Let's see what it looks like (just for info - it isn't necessary to do this) }
  (CompiledExpr as IDebugBinding).Dump(
    procedure (S: string)
    begin
      WriteLn(S);
    end);
  WriteLn;
  { Evaluate it using the original scope; in general, the scope passed here
    should be either the same as the scope passed to Compile or a superset of it }
  Result := CompiledExpr.Evaluate(Scope, nil, nil).GetValue;
  WriteLn('The result is ', Result.ToString);
  ReadLn;
end.

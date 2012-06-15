program RegisterMethodExample;
{
  Demonstrates adding a custom built-in function to the expression engine using
  TBindingMethodsFactory.RegisterMethod.

  In practice, using the WrapObject function of System.Bindings.ObjEval is an
  easier way to add items, as per the next demo.
}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Rtti, System.Bindings.EvalProtocol, System.Bindings.EvalSys,
  System.Bindings.Evaluator, System.Bindings.Methods, System.Bindings.Consts;

function CompareTextWrapper: IInvokable;
begin
  Result := MakeInvokable(
  	function(Args: TArray<IValue>): IValue
		begin
			if Length(Args) <> 2 then
				raise EEvaluatorError.CreateResFmt(@SUnexpectedArgCount, [2, Length(Args)]);
			Result := TValueWrapper.Create(CompareText(Args[0].GetValue.ToString,
			  Args[1].GetValue.ToString, loUserLocale));
		end)
end;

const
	Expr = 'CompareText("alpha", "beta")';
var
  Scope: IScope;
  Result: TValue;
begin
  //add it
  TBindingMethodsFactory.RegisterMethod(
  	TMethodDescription.Create(CompareTextWrapper, 'CompareText', '', '', True, '', nil));
  //use it
  Scope := TNestedScope.Create(BasicOperators, TBindingMethodsFactory.GetMethodScope);
  Result := Compile(Expr, Scope).Evaluate(Scope, nil, nil).GetValue;
  WriteLn('The expression ''', Expr, ''' returns ', Result.ToString); //-1
  ReadLn;
end.

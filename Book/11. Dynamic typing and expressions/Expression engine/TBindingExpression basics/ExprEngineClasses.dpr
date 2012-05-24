program ExprEngineClasses;
{
  Similar to the first expression engine demo, but using the moderately higher level classes instead.
}
{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Bindings.EvalProtocol,
  System.Bindings.Expression,
  System.Bindings.ExpressionDefaults,
  System.Bindings.Methods, //for TBindingMethodsFactory
  DemoClasses in 'DemoClasses.pas';

var
  Expr: TBindingExpression;
  ExampleComp: TExampleComponent;
  UtilsObj: TExpressionUtils;
begin
  try
    Expr := TBindingExpressionDefault.Create;
    try
      { Example of the expression engine working with no custom symbols whatsoever.
        Notice the need to call the overload of Compile that takes three arguments. }
      Expr.Source := '1 + 1';
      Expr.Compile([], [], []);
      WriteLn('1 + 1 = ', Expr.Evaluate.GetValue.ToString);

      { Example of adding in and using the stock function set. }
      Expr.Source := 'UpperCase("hello")';
      Expr.Compile(TBindingMethodsFactory.GetMethodScope);
      WriteLn('UpperCase("hello") = ', Expr.Evaluate.GetValue.ToString);

      { Example of adding in a TComponent instance. In practice, if you're doing
        such a thing, it is likely the component will have been set up at design
        time. The code here is just to show the minimum set up necessary for any
        components explicitly created at runtime, which is to ensure the
        Name property is set. Notice there's no need for an owner however. }
      ExampleComp := TExampleComponent.Create(nil);
      try
        ExampleComp.Name := 'ExampleComp';
        ExampleComp.Tag := 21; //give the expression a value to read
        Expr.Source := 'ExampleComp.Tag * 2';
        Expr.Compile([ExampleComp]);
        WriteLn('ExampleComp.Tag * 2 = ', Expr.Evaluate.GetValue.ToString);
      finally
        ExampleComp.Free;
      end;

      { Example of adding in an ordinary object, and combining it with the stock
        set of functions. Since it is parameterless, Now had to be made a property
        to ensure it would actually get called. }
      UtilsObj := TExpressionUtils.Create;
      try
        Expr.Source := 'FormatDateTime("C", Utils.Now)'; //FormatDateTime('', Now) is the same as DateTimeToStr(Now)
        Expr.Compile([], [Associate(UtilsObj, 'Utils')], [TBindingMethodsFactory.GetMethodScope]);
        WriteLn('FormatDateTime("C", Utils.Now) = ', Expr.Evaluate.GetValue.ToString);
      finally
        UtilsObj.Free;
      end;
    finally
      Expr.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.

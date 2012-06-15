unit MainForm;
{
  Demonstrates exposing objects to the expression engine. While this is a
  FireMonkey application, you could use the same code in a VCL application too.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit,
  System.Bindings.EvalProtocol;

type
  TfrmMain = class(TForm)
    lblInfo: TLabel;
    cboExpr: TComboEdit;
    btnEvalExpr: TButton;
    procedure btnEvalExprClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  strict private
    FExpressionScope: IScope;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  System.Bindings.Evaluator, System.Bindings.EvalSys, System.Bindings.ObjEval,
  System.Bindings.Methods;

{$R *.fmx}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  Objects: TDictionaryScope;
begin
  Objects := TDictionaryScope.Create;
  Objects.Map.Add('Application', WrapObject(Application));
  Objects.Map.Add('Screen', WrapObject(Screen));
  FExpressionScope := TNestedScope.Create(BasicOperators,
                        TNestedScope.Create(BasicConstants,
                          TNestedScope.Create(TBindingMethodsFactory.GetMethodScope,
                            TNestedScope.Create(Objects, WrapObject(Self)))));
end;

procedure TfrmMain.btnEvalExprClick(Sender: TObject);
var
  Output: IValue;
begin
  Output := Compile(cboExpr.Text, FExpressionScope).Evaluate(FExpressionScope, nil, nil);
  ShowMessage(Output.GetValue.ToString);
end;

end.

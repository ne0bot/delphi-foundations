unit FutureResultsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo;

type
  TURLTestRec = record
    URLToTest: string;
    TestFunc: TFunc<Integer>;
  end;

  TfrmPrimeResults = class(TForm)
    memResults: TMemo;
  end;

procedure ShowResults(const ARecs: TList<TURLTestRec>);

implementation

{$R *.fmx}

procedure ShowResults(const ARecs: TList<TURLTestRec>);
var
  Form: TfrmPrimeResults;
  Msg: TStringBuilder;
  Rec: TURLTestRec;
  ResponseCode: Integer;
begin
  Form := nil;
  Msg := TStringBuilder.Create;
  try
    for Rec in ARecs do
    begin
      Msg.Append(Rec.URLToTest + ' ');
      ResponseCode := Rec.TestFunc();
      case ResponseCode of
        200: Msg.Append('exists');
        300..399: Msg.Append('is subject to redirection');
        401: Msg.Append('requires authorisation');
        403: Msg.Append('is forbidden');
        404: Msg.Append('could not be found');
      else Msg.Append('had the response code ' + IntToStr(ResponseCode));
      end;
      Msg.AppendLine;
    end;
    Form := TfrmPrimeResults.Create(nil);
    Form.memResults.Text := Msg.ToString;
    Form.Caption := Format('%d URLs tested', [ARecs.Count]);
    with Application.MainForm do
      Form.SetBounds(Left + (Width - Form.Width) div 2, Top + (Height - Form.Height) div 2,
        Form.Width, Form.Height);
    Form.ShowModal;
  finally
    Form.Free;
    Msg.Free;
  end;
end;

end.

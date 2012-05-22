program StringEqualityTesting;

uses
  System.SysUtils,
  Vcl.Dialogs;

{$R WindowsXP.res}

procedure DoComparison(const S1, S2: string);
var
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    Builder.Append('Equality operator: ').Append(S1 = S2).AppendLine;
    Builder.Append('SameText: ').Append(SameText(S1, S2)).AppendLine;
    Builder.Append('SameText loUserLocale: ').Append(SameText(S1, S2, loUserLocale)).AppendLine;
    Builder.Append('AnsiSameText: ').Append(AnsiSameText(S1, S2));
    TaskMessageDlg('Comparing "' + S1 + '" to "' + S2 + '"', Builder.ToString,
      mtInformation, [mbOK], 0, mbOK);
  finally
    Builder.Free;
  end;
end;

begin
  DoComparison('καφενείο', 'καφενείο'); //true, true, true, true
  DoComparison('cafe', 'CAFE');         //false, true, true, true
  DoComparison('café', 'CAFÉ');         //false, false, true, true
  DoComparison('кафе', 'КАФЕ');         //false, false, true, true
end.

program NumericalCharacterLister;
{
  Small demo that finds all characters that denote a number of some sort, and
  displays information about them. And yeah, it doesn't handle RTL languages well...
}
uses
  System.SysUtils,
  System.Character,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls;

{$R WindowsXP.res} //include a manifest

var
  Ch: Char;
  Form: TForm;
  Memo: TMemo;
begin
  Form := TForm.Create(nil);
  try
    Form.Caption := 'Numerical Characters';
    Form.Font.Name := 'Arial Unicode MS'; //includes many more Unicode chars than other fonts
    Form.Position := poDefault;
    Memo := TMemo.Create(Form);
    Memo.AlignWithMargins := True;
    Memo.Align := alClient;
    Memo.Parent := Form;
    Memo.ScrollBars := ssVertical;
    Memo.Lines.BeginUpdate;
    { Cycle through all possible Char values, adding details
      of the numerical ones to the memo control. }
    for Ch := Low(Char) to High(Char) do
      if TCharacter.IsNumber(Ch) then
        Memo.Lines.Add(Format('"%s" ($%.4x) denotes %g',
          [string(Ch), Ord(Ch), TCharacter.GetNumericValue(Ch)]));
    Memo.Lines.EndUpdate;
    Form.ShowModal;
  finally
    Form.Free;
  end;
end.

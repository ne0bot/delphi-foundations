program EncodingsInfo;
{
  Displays basic info about both standard encodings and a few custom TEncoding
  implementations.
}
uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  Vcl.Forms,
  Vcl.Dialogs,
  CCR.Encodings in 'CCR.Encodings.pas';

procedure ShowEncodingsInfo;

  procedure ShowInfo(const Name: string; Encoding: TEncoding); overload;
  var
    CodePageStr, Msg: string;
    BOMByte: Byte;
    Form: TForm;
  begin
    if Encoding.CodePage = Cardinal(-1) then
      CodePageStr := 'n/a'
    else
      CodePageStr := IntToStr(Encoding.CodePage);
    Msg := '';
    for BOMByte in Encoding.GetPreamble do
      Msg := Msg + '$' + IntToHex(BOMByte, 2) + ' ';
    if Msg = '' then Msg := '(none)';
    Msg := 'CodePage = ' + CodePageStr + sLineBreak +
           'EncodingName = ' + Encoding.EncodingName + sLineBreak +
           'InSingleByte = ' + BoolToStr(Encoding.IsSingleByte, True) + sLineBreak +
           'IsStandardEncoding = ' + BoolToStr(TEncoding.IsStandardEncoding(Encoding), True) + sLineBreak +
           'GetPreamble = ' + Msg;
    Form := CreateMessageDialog(Msg, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK]);
    try
      Form.Caption := Name;
      Form.ShowModal;
    finally
      Form.Free;
      if not TEncoding.IsStandardEncoding(Encoding) then Encoding.Free;
    end;
  end;
begin
  //standard encodings
  ShowInfo('TEncoding.ASCII', TEncoding.ASCII);
  ShowInfo('TEncoding.Default', TEncoding.Default);
  ShowInfo('TEncoding.Unicode', TEncoding.Unicode); //UTF-16 LE
  ShowInfo('TEncoding.BigEndianUnicode', TEncoding.BigEndianUnicode); //UTF-16 BE
  ShowInfo('TEncoding.UTF7', TEncoding.UTF7);
  ShowInfo('TEncoding.UTF8', TEncoding.UTF8);
  //custom encoding claseses
  ShowInfo('TUTF8EncodingEx (custom)', TUTF8EncodingEx.Create([eoAllowInvalidChars]));
  ShowInfo('TUTF32Encoding (custom)', TUTF32Encoding.Create);
  //via TEncoding.GetEncoding with a Windows code page integeral identifier
  ShowInfo('TEncoding.GetEncoding(1251)', TEncoding.GetEncoding(1251));
  //via TEncoding.GetEncoding with a string identifier
  ShowInfo('TEncoding.GetEncoding(''utf-8'')', TEncoding.GetEncoding('utf-8'));
  ShowInfo('TEncoding.GetEncoding(''shift_jis'')', TEncoding.GetEncoding('shift_jis')); //OS dependent!
  MessageDlg('The next one should raise an exception...', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
  ShowInfo('TEncoding.GetEncoding(''non-existing'')', TEncoding.GetEncoding('non-existing'));
end;

begin
  try
    ShowEncodingsInfo;
  except
    on E: Exception do
      MessageDlg(E.ClassName + ': ' + E.Message, TMsgDlgType.mtError,
        [TMsgDlgBtn.mbOK], 0);
  end;
end.

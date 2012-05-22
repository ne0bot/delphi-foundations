unit SearchBufForm;
{
  Simple demo of SearchBuf usage with any TCustomEdit descendant - see DoEditSearch for
  the generalised find routine.

  The example text is stored as a resource in the EXE, which you can see in the
  IDE by going to Project|Resources and Images... Be aware this dialog can
  suffer from a small bug - when you add an item for the first time, it may not
  remember what identifer you gave the new resource until you set it for the
  second time.
}
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TfrmSearchBufDemo = class(TForm)
    Memo: TRichEdit;
    Panel1: TPanel;
    Label1: TLabel;
    edtSearchString: TEdit;
    btnFindNext: TButton;
    btnFindPrevious: TButton;
    chkMatchCase: TCheckBox;
    chkMatchWholeWord: TCheckBox;
    procedure btnFindNextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnFindPreviousClick(Sender: TObject);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure edtSearchStringKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtSearchStringKeyPress(Sender: TObject; var Key: Char);
  private
    procedure DoSearch(Down: Boolean);
  end;

var
  frmSearchBufDemo: TfrmSearchBufDemo;

implementation

uses System.StrUtils;

{$R *.dfm}

{ Generalised application of the SearchBuf RTL routine to implement find next/previous
  functionality for a VCL TCustomEdit descedant (e.g. TEdit, TMemo, TRichEdit). Works
  around a rich edit control quirk - its SelStart property assumes only 1 character per
  hard line break where its Text property returns the Windows-standard CRLF sequence. }
procedure DoEditSearch(Control: TCustomEdit; const SearchStr: string;
  MatchCase, MatchWholeWord, Down: Boolean);
var
  Options: TStringSearchOptions;
  Text: string;
  StartPtr, EndPtr: PChar;
begin
  if SearchStr = '' then
  begin
    Beep;
    Exit;
  end;
  if Down then Options := [soDown] else Options := [];
  if MatchCase then Include(Options, soMatchCase);
  if MatchWholeWord then Include(Options, soWholeWord);
  Text := Control.Text;
  if Control is TCustomRichEdit then Text := AdjustLineBreaks(Text, tlbsLF);
  StartPtr := PChar(Text);
  EndPtr := SearchBuf(StartPtr, Length(Text), Control.SelStart, Control.SelLength,
    SearchStr, Options);
  if EndPtr = nil then
  begin
    MessageDlg('Search string not found.', mtInformation, [mbOK], 0);
    Exit;
  end;
  Control.SelStart := EndPtr - StartPtr;
  Control.SelLength := Length(SearchStr);
end;

procedure TfrmSearchBufDemo.FormCreate(Sender: TObject);
var
  ResStream: TResourceStream;
begin
  ResStream := TResourceStream.Create(HInstance, 'ExampleText', RT_RCDATA);
  try
    Memo.Lines.LoadFromStream(ResStream);
  finally
    ResStream.Free;
  end;
end;

procedure TfrmSearchBufDemo.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
  case Msg.CharCode of
    Ord('F'):
      if KeyDataToShiftState(Msg.KeyData) = [ssCtrl] then
      begin
        edtSearchString.SetFocus;
        Handled := True;
      end;
    VK_F3:
      if KeyDataToShiftState(Msg.KeyData) = [ssShift] then
        btnFindPrevious.Click
      else
        btnFindNext.Click;
  end;
end;

procedure TfrmSearchBufDemo.edtSearchStringKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    if ssShift in Shift then
      btnFindPrevious.Click
    else
      btnFindNext.Click;
  end;
end;

procedure TfrmSearchBufDemo.edtSearchStringKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then Key := #0; //cancel the 'invalid character' beep or sound
end;

procedure TfrmSearchBufDemo.DoSearch(Down: Boolean);
begin
  DoEditSearch(Memo, edtSearchString.Text, chkMatchCase.Checked,
    chkMatchWholeWord.Checked, Down);
end;

procedure TfrmSearchBufDemo.btnFindNextClick(Sender: TObject);
begin
  DoSearch(True);
end;

procedure TfrmSearchBufDemo.btnFindPreviousClick(Sender: TObject);
begin
  DoSearch(False);
end;

end.

unit UnicodeNormForm;
{
  Unicode text can be both 'precomposed' and 'decomposed', decomposed text being
  when accents and the like are encoded as separate Char values. As displayed it
  shouldn't matter however.

  Since the Delphi RTL does not provide any routines for precomposing or
  decomposing a string, we need to drop down to the native API, calling
  FoldString on Windows and CFStringNormalize on OS X.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Effects, FMX.Objects, FMX.Edit;

type
  TfrmUnicodeNorm = class(TForm)
    Label1: TLabel;
    edtText: TEdit;
    Label2: TLabel;
    lblPrecomposed: TLabel;
    Label4: TLabel;
    lblDecomposed: TLabel;
    Line1: TLine;
    ShadowEffect1: TShadowEffect;
    Label3: TLabel;
    lblPrecomposedLen: TLabel;
    Label6: TLabel;
    lblDecomposedLen: TLabel;
    procedure edtTextChangeTracking(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  frmUnicodeNorm: TfrmUnicodeNorm;

implementation

uses
  {$IF Defined(MSWINDOWS)}
  Winapi.Windows;
  {$ELSEIF Defined(MACOS)}
  Macapi.CoreFoundation;
  {$ELSE}
  NotImplementedForPlatform;
  {$IFEND}

{$R *.fmx}

{$IFDEF MACOS}
function NormalizeViaCF(const S: string; NormForm: CFStringNormalizationForm): string;
var
  Ref: Pointer;
  R: CFRange;
begin
  if S = '' then Exit('');
  Ref := CFStringCreateMutable(nil, 0);
  CFStringAppendCharacters(Ref, PChar(S), Length(S));
  CFStringNormalize(Ref, NormForm);
  R.location := 0;
  R.length := CFStringGetLength(Ref);
  SetLength(Result, R.length);
  CFStringGetCharacters(Ref, R, PChar(Result));
end;
{$ENDIF}

function EnsurePrecomposed(const S: string): string;
begin
  if S = '' then Exit('');
{$IF Defined(MSWINDOWS)}
  SetLength(Result, Length(S));
  SetLength(Result, FoldString(MAP_PRECOMPOSED, PChar(S), Length(S),
    PChar(Result), Length(S)));
{$ELSEIF Defined(MACOS)}
  Result := NormalizeViaCF(S, kCFStringNormalizationFormC);
{$IFEND}
end;

function EnsureDecomposed(const S: string): string;
begin
  if S = '' then Exit('');
{$IF Defined(MSWINDOWS)}
  SetLength(Result, FoldString(MAP_COMPOSITE, PChar(S), Length(S), nil, 0));
  if FoldString(MAP_COMPOSITE, PChar(S), Length(S), PChar(Result), Length(Result)) = 0 then
    RaiseLastOSError;
{$ELSEIF Defined(MACOS)}
  Result := NormalizeViaCF(S, kCFStringNormalizationFormD);
{$IFEND}
end;

procedure TfrmUnicodeNorm.FormCreate(Sender: TObject);
begin
  edtTextChangeTracking(nil);
end;

procedure TfrmUnicodeNorm.edtTextChangeTracking(Sender: TObject);
var
  Source, Dest: string;
begin
  Source := edtText.Text;
  Dest := EnsurePrecomposed(Source);
  lblPrecomposed.Text := Dest;
  lblPrecomposedLen.Text := IntToStr(Length(Dest));
  Dest := EnsureDecomposed(Source);
  lblDecomposed.Text := Dest;
  lblDecomposedLen.Text := IntToStr(Length(Dest));
end;

end.

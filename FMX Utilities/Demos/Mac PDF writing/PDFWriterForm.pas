unit PDFWriterForm;
{
  Demo of TPDFFileWriter that creates a two-page PDF, the contents for which coming
  from the two tabs. Page 1 simply asks the controls to draw themselves onto the
  PDF canvas; page 2, in contrast, writes some text explicitly. Note that since
  TPDFFileWriter's purpose is to expose an OS X API, the demo doesn't run on Windows!

  Chris Rolliston, August 2012
  http://delphifoundations.com/
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.Memo, FMX.TabControl, FMX.Effects;

type
  TfrmPDFWriter = class(TForm)
    TabControl1: TTabControl;
    tabText: TTabItem;
    tabCoverPage: TTabItem;
    lyoCoverPage: TLayout;
    Image1: TImage;
    txtTitle: TText;
    btnCreatePDF: TButton;
    lyoPage2: TRectangle;
    rctLeftMargin: TRectangle;
    splLeftMargin: TSplitter;
    rctTopMargin: TRectangle;
    splTopMargin: TSplitter;
    splRightMargin: TSplitter;
    rctRightMargin: TRectangle;
    rctBottomMargin: TRectangle;
    splBottomMargin: TSplitter;
    memContent: TMemo;
    StyleBookWithTSplitterFix: TStyleBook;
    GlowEffect1: TGlowEffect;
    txtAuthor: TText;
    GlowEffect2: TGlowEffect;
    procedure btnCreatePDFClick(Sender: TObject);
    procedure memContentApplyStyleLookup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  strict private
    {$IFDEF VER230}
    FSavedDoContentPaintWithCache: TOnPaintEvent;
    procedure DoContentPaintWithCacheFix(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    {$ENDIF}
  end;

var
  frmPDFWriter: TfrmPDFWriter;

implementation

{$R *.fmx}

uses
  Posix.Stdlib, System.StrUtils,
  {$IFDEF VER230}CCR.FMXNativeDlgs{$ELSE}FMX.Text{$ENDIF}, CCR.MacPDFWriter;

type
  TMemoAccess = class(TMemo);

procedure ShellOpen(const FileName: string);
begin
  _system(PAnsiChar(UTF8String('open "' +
    ReplaceStr(ReplaceStr(FileName, '\', '\\'), '"', '\"') + '"')));
end;

procedure TfrmPDFWriter.btnCreatePDFClick(Sender: TObject);
var
  {$IFNDEF VER230}
  Layout: TTextLayout;
  {$ENDIF}
  R: TRectF;
  Writer: TPDFFileWriter;
begin
  Writer := TPDFFileWriter.Create(ExpandFileName('~/Documents/Test.pdf'));
  try
    //assign some metadata
    Writer.Author := txtAuthor.Text;
    Writer.Creator := ExtractFileName(GetModuleName(0)); //name of the application
    Writer.Keywords.Add('test');
    Writer.Keywords.Add('demo');
    Writer.Subject := 'My first PDF file';
    Writer.Title := txtTitle.Text;
    //set the 'security' settings
    Writer.OwnerPassword := 'SuperSecretPassword';
    Writer.AllowPrinting := False;
    //set the page size
    Writer.Width := lyoCoverPage.Width;
    Writer.Height := lyoCoverPage.Height;
    //start the actual writing, output the cover page, and start a new page for the memo text
    Writer.BeginDoc;
    lyoCoverPage.PaintTo(Writer.Canvas, lyoCoverPage.ClipRect);
    Writer.NewPage;
    //get the text bounds
    R := TMemoAccess(memContent).ContentRect;
    R.Offset(memContent.Position.X, memContent.Position.Y);
    //actually output the text
    {$IFDEF VER230} //XE2
    Writer.Canvas.Fill.Assign(memContent.FontFill);
    Writer.Canvas.Font.Assign(memContent.Font);
    Writer.Canvas.FillText(R, memContent.Text, True, memContent.Opacity, [],
      memContent.TextAlign, TTextAlign.taLeading);
    {$ELSE}         //XE3+
    Layout := TTextLayoutManager.TextLayoutByCanvas(Writer.Canvas.ClassType).Create(Writer.Canvas);
    try
      Layout.BeginUpdate;
      Layout.TopLeft := R.TopLeft;
      Layout.MaxSize := PointF(R.Right - R.Left, R.Bottom - R.Top);
      Layout.Text := memContent.Text;
      Layout.WordWrap := memContent.WordWrap;
      Layout.EndUpdate;
      Layout.RenderLayout(Writer.Canvas);
    finally
      Layout.Free;
    end;
    {$ENDIF}
    //finish up
    Writer.EndDoc;
    if MessageDlg('Finished writing PDF file. Open now?', TMsgDlgType.mtConfirmation, mbYesNo, 0) = mrYes then
      ShellOpen(Writer.FileName);
  finally
    Writer.Free;
  end;
end;

procedure TfrmPDFWriter.FormCreate(Sender: TObject);
begin
  {$IFDEF VER240}
  TabControl1.TabIndex := 1;
  TabControl1.TabIndex := 0;
  {$ENDIF}
end;

procedure TfrmPDFWriter.memContentApplyStyleLookup(Sender: TObject);
var
  Resource: TFMXObject;
begin
  //remove the memo's border
  Resource := memContent.FindStyleResource('background');
  if Resource is TRectangle then TRectangle(Resource).StrokeDash := TStrokeDash.sdDash;
  //work around XE2 visual bug (http://qc.embarcadero.com/wc/qcmain.aspx?d=105661)
  {$IFDEF VER230}
  Resource := memContent.FindStyleResource('content');
  if (Resource is TControl) then
  begin
    FSavedDoContentPaintWithCache := TControl(Resource).OnPaint;
    TControl(Resource).OnPaint := DoContentPaintWithCacheFix;
  end;
  {$ENDIF}
end;

{$IFDEF VER230}
procedure TfrmPDFWriter.DoContentPaintWithCacheFix(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  Canvas.Fill.Color := TAlphaColors.White;
  FSavedDoContentPaintWithCache(Sender, Canvas, ARect);
end;
{$ENDIF}

end.

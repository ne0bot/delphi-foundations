unit PathsForm;
{
  Demo of VCL and Windows API interop, in this case the fact a VCL TCanvas wraps an API level
  HDC (= handle to a 'device context'), and can be worked upon using DC functions.
}
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TfrmPathsDemo = class(TForm)
    pbxText: TPaintBox;
    procedure pbxTextPaint(Sender: TObject);
  end;

var
  frmPathsDemo: TfrmPathsDemo;

implementation

{$R *.dfm}

uses Vcl.GraphUtil;

procedure DrawGradientText(ACanvas: TCanvas; const AText: string;
  X, Y: Integer; AStartColor, AEndColor: TColor;
  ADirection: TGradientDirection);
var
  DC: HDC;
  R: TRect;
  SaveIndex: Integer;
begin
  { Drop down to the Windows API to save the state of the canvas }
  DC := ACanvas.Handle;
  SaveIndex := SaveDC(DC);
  try
    { Compose the destination rectangle }
    R.Left := X;
    R.Top := Y;
    R.Right := X + ACanvas.TextWidth(AText);
    R.Bottom := Y + ACanvas.TextHeight(AText);
    { Using a mixture of direct API and VCL calls, create the path, select it as the clipping
      region, and fill it with the gradient }
    BeginPath(DC);
    ACanvas.TextOut(R.Left, R.Top, AText);
    EndPath(DC);
    SelectClipPath(DC, RGN_DIFF);
    GradientFillCanvas(ACanvas, AStartColor, AEndColor, R, ADirection);
  finally
    { Drop down to the API once more to restore the original state of the canvas }
    RestoreDC(DC, SaveIndex)
  end;
end;

procedure TfrmPathsDemo.pbxTextPaint(Sender: TObject);
var
  Size: TSize;
  TextToDraw: string;
begin
  TextToDraw := 'Paths are neat';
  pbxText.Canvas.Font.Name := 'Arial Black';
  pbxText.Canvas.Font.Size := 36;
  Size := pbxText.Canvas.TextExtent(TextToDraw);
  DrawGradientText(pbxText.Canvas, TextToDraw, (pbxText.Width - Size.cx) div 2,
    (pbxText.Height - Size.cy) div 2, clYellow, clRed, gdVertical);
end;

end.

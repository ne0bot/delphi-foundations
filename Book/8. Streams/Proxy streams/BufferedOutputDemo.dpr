program BufferedOutputDemo;
{                   //!!!REDO TO BE LIKE BufferedInputDemo
  This program benchmarks an output stream buffered with a proxy against an
  unbuffered output stream, the test being to save a screenshot to a PNG image
  file. On my computer, the proxy makes no differenceUnlike the very artificial buffered input stream test
}
{$APPTYPE CONSOLE}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  Vcl.Graphics,
  Vcl.Imaging.PngImage,
  BufferedStreams in 'BufferedStreams.pas';

procedure RunTest(const TestName: string; const Test: TProc);
var
  Stopwatch: TStopwatch;
begin
  WriteLn('Testing ', TestName, '...');
  Stopwatch := TStopwatch.StartNew;
  Test;
  Stopwatch.Stop;
  Writeln('  Took ', Stopwatch.ElapsedMilliseconds, 'ms');
end;

var
  Bitmap: TBitmap;
  DestFileName: string;
  Png: TPngImage;
  ScreenDC: HDC;
  I, ScreenWidth, ScreenHeight: Integer;
begin
  DestFileName := ChangeFileExt(ParamStr(0), '.png');
  Png := nil;
  ScreenWidth := GetSystemMetrics(SM_CXSCREEN);
  ScreenHeight := GetSystemMetrics(SM_CYSCREEN);
  ScreenDC := 0;
  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf32bit;
    Bitmap.SetSize(ScreenWidth * 2, ScreenHeight * 2);
    ScreenDC := GetDC(0);
    StretchBlt(Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, ScreenDC,
      0, 0, ScreenWidth, ScreenHeight, SRCCOPY);
    Png := TPngImage.Create;
    Png.Assign(Bitmap);
  finally
    Bitmap.Free;
    if ScreenDC <> 0 then ReleaseDC(0, ScreenDC);
  end;
  try
    for I := 1 to 3 do
    begin
      Writeln('*** ROUND ', I, ' ***');
      RunTest('TPngImage.SaveToFile',
        procedure
        begin
          Png.SaveToFile(DestFileName);
        end);
      RunTest('TBufferedOutputStream',
        procedure
        var
          Stream: TBufferedOutputStream;
        begin
          Stream := TBufferedOutputStream.Create(TFileStream.Create(DestFileName, fmCreate), True);
          try
            Png.SaveToStream(Stream);
          finally
            Stream.Free;
          end;
        end);
      WriteLn;
    end;
  finally
    Png.Free;
  end;
  Readln;
end.

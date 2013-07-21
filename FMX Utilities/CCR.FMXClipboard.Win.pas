{**************************************************************************************}
{                                                                                      }
{ FMX Utilities: TClipboard (Windows backend)                                          }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2012-13 Chris Rolliston. All Rights Reserved.      }
{                                                                                      }
{**************************************************************************************}

unit CCR.FMXClipboard.Win;
{
  Part of a FireMonkey TClipboard implementation for Windows and OS X. The relation of this unit
  to CCR.FMXClipboard.pas is akin to the relation between FMX.Platform.Win.pas to FMX.Platform.pas.
}
interface

{$IFDEF MSWINDOWS}
uses
  Winapi.Windows, System.Types, System.SysUtils, System.Classes, FMX.Types,
  {$IFNDEF VER230}FMX.PixelFormats,{$ENDIF} CCR.FMXClipboard;

type
  TWinClipboard = class(TClipboard)
  strict private
    FOwnerWnd: HWND;
  strict protected
    constructor CreateForSingleton(out cfText, cfBitmap, cfTIFF: TClipboardFormat); override;
    procedure DoAssignBitmap(ABitmap: TBitmap); override;
    procedure DoAssignBuffer(AFormat: TClipboardFormat; const ABuffer; ASize: Integer); override;
    procedure DoGetBitmap(ABitmap: TBitmap); override;
    procedure DoClear; override;
    procedure DoOpen; override;
    procedure DoClose; override;
    function DoGetAsText: string; override;
    procedure DoSetAsText(const Value: string); override;
    function DoToBytes(AFormat: TClipboardFormat): TBytes; override;
  public
    function GetFormats: TArray<TClipboardFormat>; override;
    class function GetFormatName(AFormat: TClipboardFormat): string; override;
    function HasFormat(AFormat: TClipboardFormat): Boolean; override;
    function HasFormat(const AFormats: array of TClipboardFormat; out Matched: TClipboardFormat): Boolean; override;
    class function RegisterFormat(const AName: string): TClipboardFormat; override;
  end;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
uses
  Winapi.ShellApi, System.RTLConsts;

function GetPriorityClipboardFormat(const paFormatPriorityList;
  cFormats: Integer): Integer; stdcall; external user32;

var
  WndClass: TWndClassEx = (cbSize: SizeOf(WndClass); lpszClassName: 'FMXClipboard');

constructor TWinClipboard.CreateForSingleton(out cfText, cfBitmap, cfTIFF: TClipboardFormat);
begin
  inherited;
  cfText := CF_TEXT;
  cfBitmap := CF_BITMAP;
  cfTIFF := CF_TIFF;
  WndClass.lpfnWndProc := @DefWindowProc;
  WndClass.hInstance := HInstance;
  RegisterClassEx(WndClass);
  FOwnerWnd := CreateWindow(WndClass.lpszClassName, 'FMXClipboard', 0, 0, 0, 0, 0, HWND_MESSAGE, 0, 0, nil);
end;

class function TWinClipboard.RegisterFormat(const AName: string): TClipboardFormat;
begin
  Result := RegisterClipboardFormat(PChar(AName));
end;

procedure TWinClipboard.DoGetBitmap(ABitmap: TBitmap);
var
  FileName: array[0..MAX_PATH] of Char;
  Header: TBitmapFileHeader;
  DataHandle: HGLOBAL;
  BitmapInfoPtr: PBitmapInfoHeader;
  Stream: TMemoryStream;
begin
  //is there a file name on the clipboard that points to a graphic?
  DataHandle := GetClipboardData(CF_HDROP);
  if DataHandle <> 0 then
  begin
    DragQueryFile(DataHandle, 0, FileName, Length(FileName));
    if TryLoadBitmapFromFile(ABitmap, FileName) then Exit;
  end;
  //test for actual image data next
  DataHandle := GetClipboardData(CF_DIB);
  if DataHandle = 0 then BitmapInfoPtr := nil else BitmapInfoPtr := GlobalLock(DataHandle);
  if BitmapInfoPtr = nil then
  begin
    ABitmap.SetSize(0, 0);
    Exit;
  end;
  Stream := TMemoryStream.Create;
  try
    FillChar(Header, SizeOf(Header), 0);
    Header.bfType := $4D42;
    Header.bfSize := SizeOf(Header) + GlobalSize(DataHandle);
    Header.bfOffBits := SizeOf(Header) + BitmapInfoPtr.biSize;
    Stream.WriteBuffer(Header, SizeOf(Header));
    Stream.WriteBuffer(BitmapInfoPtr^, Header.bfSize - SizeOf(Header));
    Stream.Position := 0;
    ABitmap.LoadFromStream(Stream);
  finally
    GlobalUnlock(DataHandle);
    Stream.Free;
  end;
end;

procedure TWinClipboard.DoAssignBitmap(ABitmap: TBitmap);
var
  BitsSize: Integer;
  MemObj: HGLOBAL;
  Ptr: PBitmapInfoHeader;
  {$IF DECLARED(TPixelFormat)}
  R: TRectF;
  Rec: TBitmapData;
  TempBitmap: TBitmap;
  {$ENDIF}
begin
  BitsSize := ABitmap.Width * ABitmap.Height * 4;
  MemObj := GlobalAlloc(GMEM_MOVEABLE and GMEM_DDESHARE, SizeOf(TBitmapInfoHeader) + BitsSize);
  if MemObj = 0 then RaiseLastOSError;
  Ptr := GlobalLock(MemObj);
  if Ptr = nil then
  begin
    GlobalFree(MemObj);
    RaiseLastOSError;
  end;
  //fill out the info header
  FillChar(Ptr^, SizeOf(Ptr^), 0);
  Ptr.biSize := SizeOf(TBitmapInfoHeader);
  Ptr.biPlanes := 1;
  Ptr.biBitCount := 32;
  Ptr.biCompression := BI_RGB;
  Ptr.biWidth := ABitmap.Width;
  if Ptr.biWidth <= 0 then Ptr.biWidth := 1;
  Ptr.biHeight := -ABitmap.Height;
  if Ptr.biHeight >= 0 then Ptr.biHeight := -1;
  //copy over the image bits
  Inc(Ptr);
  if BitsSize <> 0 then
  {$IF NOT DECLARED(TempBitmap)}
    Move(ABitmap.StartLine^, Ptr^, BitsSize);
  {$ELSE}
  begin
    TempBitmap := ABitmap;
    try
      if ABitmap.PixelFormat <> TPixelFormat.pfA8R8G8B8 then
      begin
        R := RectF(0, 0, ABitmap.Width, ABitmap.Height);
        TempBitmap := TBitmap.Create(ABitmap.Width, ABitmap.Height);
        TempBitmap.Canvas.BeginScene;
        TempBitmap.Canvas.DrawBitmap(ABitmap, R, R, 1);
        TempBitmap.Canvas.EndScene;
      end;
      TempBitmap.Map(TMapAccess.maRead, Rec);
      Move(Rec.Data^, Ptr^, BitsSize);
      TempBitmap.Unmap(Rec);
    finally
      if TempBitmap <> ABitmap then TempBitmap.Free;
    end;
  end;
  {$IFEND}
  GlobalUnlock(MemObj);
  //assign the completed DIB memory object to the clipboard
  if SetClipboardData(CF_DIB, MemObj) = 0 then
  begin
    GlobalFree(MemObj);
    RaiseLastOSError;
  end;
end;

procedure TWinClipboard.DoAssignBuffer(AFormat: TClipboardFormat; const ABuffer; ASize: Integer);
var
  DataHandle: HGLOBAL;
  Ptr: PByte;
begin
  DataHandle := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, ASize);
  if DataHandle = 0 then RaiseLastOSError;
  Ptr := GlobalLock(DataHandle);
  try
    if Ptr = nil then
    begin
      GlobalFree(DataHandle);
      RaiseLastOSError;
    end;
    Move(ABuffer, Ptr^, ASize);
  finally
    GlobalUnlock(DataHandle);
  end;
  SetClipboardData(AFormat, DataHandle);
end;

procedure TWinClipboard.DoClear;
begin
  EmptyClipboard;
end;

procedure TWinClipboard.DoClose;
begin
  CloseClipboard;
end;

function TWinClipboard.DoGetAsText: string;
var
  Data: THandle;
  Ptr: PChar;
begin
  Data := GetClipboardData(CF_UNICODETEXT);
  if Data = 0 then Exit('');
  Ptr := GlobalLock(Data);
  try
    Result := Ptr;
  finally
    GlobalUnlock(Data);
  end;
end;

procedure TWinClipboard.DoOpen;
begin
  OpenClipboard(FOwnerWnd);
end;

function TWinClipboard.GetFormats: TArray<TClipboardFormat>;
var
  Count: Integer;
  Format: UINT;
begin
  Count := 0;
  Format := 0;
  Open;
  try
    repeat
      Format := EnumClipboardFormats(Format);
      if Format = 0 then Break;
      if Length(Result) = Count then SetLength(Result, Count + 8);
      Result[Count] := Format;
      Inc(Count);
    until False;
  finally
    Close;
  end;
  SetLength(Result, Count);
end;

class function TWinClipboard.GetFormatName(AFormat: TClipboardFormat): string;
var
  Buffer: array[Byte] of Char;
begin
  case AFormat of
    CF_TEXT: Result := 'Text';
    CF_BITMAP: Result := 'Bitmap';
    CF_METAFILEPICT: Result := 'WMF';
    CF_SYLK: Result := 'SYLK';
    CF_DIF: Result := 'DIF';
    CF_TIFF: Result := 'TIFF';
    CF_OEMTEXT: Result := 'OEM Text';
    CF_DIB: Result := 'DIB';
    CF_PALETTE: Result := 'Palette';
    CF_PENDATA: Result := 'Pen Data';
    CF_RIFF: Result := 'RIFF';
    CF_WAVE: Result := 'Wave';
    CF_UNICODETEXT: Result := 'Unicode Text';
    CF_ENHMETAFILE: Result := 'EMF';
    CF_HDROP: Result := 'HDROP';
    CF_LOCALE: Result := 'Locale';
  else
    SetString(Result, Buffer, GetClipboardFormatName(AFormat, Buffer, Length(Buffer)));
    if Result = '' then raise EArgumentException.CreateRes(@sArgumentInvalid);
  end;
end;

function TWinClipboard.HasFormat(AFormat: TClipboardFormat): Boolean;
begin
  Result := IsClipboardFormatAvailable(AFormat);
end;

function TWinClipboard.HasFormat(const AFormats: array of TClipboardFormat;
  out Matched: TClipboardFormat): Boolean;
begin
  Matched := TClipboardFormat(GetPriorityClipboardFormat(AFormats[0], Length(AFormats)));
  if Integer(Matched) = -1 then Matched := 0;
  Result := (Matched <> 0);
end;

procedure TWinClipboard.DoSetAsText(const Value: string);
begin
  DoAssignBuffer(CF_UNICODETEXT, PChar(Value)^, Succ(Length(Value)) * SizeOf(Char));
end;

function TWinClipboard.DoToBytes(AFormat: TClipboardFormat): TBytes;
var
  Handle: HGLOBAL;
  Ptr: PByte;
begin
  Handle := GetClipboardData(AFormat);
  if Handle = 0 then Exit(nil);
  Ptr := GlobalLock(Handle);
  try
    SetLength(Result, GlobalSize(Handle));
    if Result <> nil then Move(Ptr^, Result[0], Length(Result));
  finally
    GlobalUnlock(Handle);
  end;
end;
{$ENDIF}

end.

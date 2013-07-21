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
  Part of a FireMonkey TClipboard implementation for Windows and OS X. The relation of
  this unit to CCR.FMXClipboard.pas is akin to the relation between FMX.Platform.Win.pas
  to FMX.Platform.pas.

  History
  - July 2013: fixed bitmap pitch issue + switched to reading and writing v5 DIBs.
  - August 2012: initial release.
}
interface

{$IFDEF MSWINDOWS}
uses
  Winapi.Windows, System.Types, System.SysUtils, System.Classes, System.UITypes,
  FMX.Types, {$IF FireMonkeyVersion >= 17}FMX.PixelFormats,{$IFEND}
  CCR.FMXClipboard;

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
    function DoOpen: Boolean; override;
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

resourcestring
  SCannotMapBitmap = 'Map method of TBitmap failed';
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
uses
  Winapi.ShellApi, System.Math, System.RTLConsts, System.UIConsts;

{$IF FireMonkeyVersion < 17}
type
  TPixelFormat = (pfUnknown, pfA8R8G8B8);

  TBitmapData = record
    Data: Pointer;
    Pitch: Integer;
    PixelFormat: TPixelFormat;
  end;

  TMapAccess = (maRead, maWrite, maReadWrite);

  TBitmapHelper = class helper for TBitmap
    function Map(const Access: TMapAccess; var Data: TBitmapData): Boolean;
    procedure Unmap(var Data: TBitmapData);
  end;

function TBitmapHelper.Map(const Access: TMapAccess; var Data: TBitmapData): Boolean;
begin
  Data.Data := StartLine;
  Data.Pitch := Width * 4;
  Data.PixelFormat := TPixelFormat.pfA8R8B8B8;
  Result := True;
end;

procedure TBitmapHelper.Unmap(var Data: TBitmapData);
begin
end;
{$IFEND}

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
  BitmapInfoPtr: PBitmapV5Header;
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
  DataHandle := GetClipboardData(CF_DIBV5);
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
    Header.bfOffBits := SizeOf(Header) + BitmapInfoPtr.bV5Size;
    Stream.WriteBuffer(Header, SizeOf(Header));
    Stream.WriteBuffer(BitmapInfoPtr^, Header.bfSize - SizeOf(Header));
    Stream.Position := 0;
    ABitmap.LoadFromStream(Stream);
  finally
    GlobalUnlock(DataHandle);
    Stream.Free;
  end;
end;

procedure DoAddBitmapDataToClipboard(SourceBits: TBitmapData);
var
  DestPtr: PBitmapV5Header;
  DestPitch, Y: Integer;
  SourceLine: PAlphaColorRec;
  MemObj: HGLOBAL;
begin
  DestPitch := SourceBits.Width * 4;
  MemObj := GlobalAlloc(GMEM_MOVEABLE and GMEM_DDESHARE,
    SizeOf(TBitmapV5Header) + DestPitch * SourceBits.Height);
  if MemObj = 0 then RaiseLastOSError;
  DestPtr := GlobalLock(MemObj);
  if DestPtr = nil then
  begin
    GlobalFree(MemObj);
    RaiseLastOSError;
  end;
  ZeroMemory(DestPtr, SizeOf(TBitmapV5Header));
  DestPtr.bV5Size := SizeOf(TBitmapV5Header);
  DestPtr.bV5Planes := 1;
  DestPtr.bV5Width := Max(1, SourceBits.Width);
  DestPtr.bV5Height := -Max(1, SourceBits.Height); //top-down DIB
  DestPtr.bV5SizeImage := DestPitch * SourceBits.Height;
  SourceLine := SourceBits.Data;
  DestPtr.bV5Compression := BI_BITFIELDS;
  DestPtr.bV5BitCount := 32;
  DestPtr.bV5RedMask   := $00FF0000;
  DestPtr.bV5GreenMask := $0000FF00;
  DestPtr.bV5BlueMask  := $000000FF;
  DestPtr.bV5AlphaMask := $FF000000;
  Inc(DestPtr);
  for Y := 0 to SourceBits.Height - 1 do
  begin
    Move(SourceLine^, DestPtr^, DestPitch);
    Inc(PByte(SourceLine), SourceBits.Pitch);
    Inc(PByte(DestPtr), DestPitch);
  end;
  GlobalUnlock(MemObj);
  //assign the completed DIB memory object to the clipboard
  if SetClipboardData(CF_DIBV5, MemObj) = 0 then
  begin
    GlobalFree(MemObj);
    RaiseLastOSError;
  end;
end;

procedure TWinClipboard.DoAssignBitmap(ABitmap: TBitmap);
var
  SourceBits: TBitmapData;
begin
  if not ABitmap.Map(TMapAccess.maRead, SourceBits) then
    raise EInvalidOperation.CreateRes(@SCannotMapBitmap);
  try
    DoAddBitmapDataToClipboard(SourceBits);
  finally
    ABitmap.Unmap(SourceBits);
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

function TWinClipboard.DoOpen: Boolean;
begin
  Result := OpenClipboard(FOwnerWnd);
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
    CF_WAVE: Result := 'Wave Audio';
    CF_UNICODETEXT: Result := 'Unicode Text';
    CF_ENHMETAFILE: Result := 'EMF';
    CF_HDROP: Result := 'File List (HDROP)';
    CF_LOCALE: Result := 'Locale Identifier';
    CF_DIBV5: Result := 'DIB v5';
    CF_OWNERDISPLAY: Result := 'Owner-Drawn Display';
    CF_DSPTEXT: Result := 'Display Text';
    CF_DSPBITMAP: Result := 'Display Bitmap';
    CF_DSPMETAFILEPICT: Result := 'Display WMF';
    CF_DSPENHMETAFILE: Result := 'Display EMF';
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
var
  Item: Integer;
begin
  Item := GetPriorityClipboardFormat(AFormats[0], Length(AFormats));
  Result := (Item > 0);
  if Result then Matched := TClipboardFormat(Item) else Matched := 0;
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

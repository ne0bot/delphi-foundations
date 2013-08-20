{**************************************************************************************}
{                                                                                      }
{ FMX Utilities: TClipboard (OS X/iOS shared code)                                     }
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

unit CCR.FMXClipboard.Apple;

interface

{$IFDEF MACOS}
uses
  {$IFDEF IOS}
  iOSapi.CocoaTypes, iOSapi.CoreGraphics, iOSapi.Foundation,
  {$ELSE}
  Macapi.CocoaTypes, Macapi.CoreGraphics, Macapi.Foundation,
  {$ENDIF}
  System.Math, System.SysUtils, System.Classes, System.Generics.Collections, System.UITypes,
  Macapi.ObjectiveC, Macapi.CoreFoundation, FMX.Types, CCR.FMXClipboard;

type
  TAppleClipboard = class abstract(TClipboard)
  private class var
    FRegisteredFormats: TDictionary<string, TClipboardFormat>;
    class constructor Create;
    class destructor Destroy;
  protected
    class function SameFormat(const Format1, Format2: TClipboardFormat): Boolean; override;
    class function RegisterStdFormat(const StdFormat: NSString): TClipboardFormat; static;
    function TypesToFormats(const Types: NSArray): TArray<TClipboardFormat>;
  public
    class var MaxBitmapSize: LongWord;
    function GetFormatName(const AFormat: TClipboardFormat): string; override;
    function RegisterFormat(const AName: string): TClipboardFormat; override;
  end;

function CFStringGetValue(const CFStr: CFStringRef): string;
function CGRectMake(x: Single; y: Single; width: Single; height: Single): CGRect; inline;
procedure CopyCGImageToBitmap(const Source: CGImageRef; const Dest: TBitmap);
function CreateCGImageFromBitmap(const ABitmap: TBitmap): CGImageRef;
function FormatToNSString(const AFormat: TClipboardFormat): NSString; inline;
function FormatsToNSArray(const AFormats: array of TClipboardFormat): NSArray;
function NSDataToBytes(const Data: NSData): TBytes;
function NSStringGetValue(const NSStr: NSString): string; inline;
{$ENDIF}

implementation

{$IFDEF MACOS}
function CFStringGetValue(const CFStr: CFStringRef): string;
var
  Range: CFRange;
begin
  if CFStr = nil then Exit('');
  Range.location := 0;
  Range.length := CFStringGetLength(CFStr);
  SetLength(Result, Range.length);
  CFStringGetCharacters(CFStr, Range, PChar(Result));
end;

function CGRectMake(x: Single; y: Single; width: Single; height: Single): CGRect;
begin
  Result.origin.x := x;
  Result.origin.y := y;
  Result.size.width := width;
  Result.size.height := height;
end;

procedure CopyCGImageToBitmap(const Source: CGImageRef; const Dest: TBitmap);
var
  ColorSpace: CGColorSpaceRef;
  Context: CGContextRef;
  MapRec: TBitmapData;
  W, H: LongWord;
  C: Extended;
begin
  W := CGImageGetWidth(Source);
  H := CGImageGetHeight(Source);
  C := Max(W, H) / TAppleClipboard.MaxBitmapSize;
  if C > 1 then
  begin
    W := Round(W / C);
    H := Round(H / C);
  end;
  Dest.SetSize(W, H);
  Dest.Clear(TAlphaColors.Null);
  ColorSpace := nil;
  Context := nil;
  if not Dest.Map(TMapAccess.maWrite, MapRec) then
    raise EInvalidOperation.CreateRes(@SCannotMapBitmap);
  try
    ColorSpace := CGColorSpaceCreateDeviceRGB;
    Context := CGBitmapContextCreate(MapRec.Data, Dest.Width, Dest.Height, 8,
      MapRec.Pitch, ColorSpace, kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big);
    if Context = nil then RaiseLastOSError;
    CGContextDrawImage(Context, CGRectMake(0, 0, Dest.Width, Dest.Height), Source);
  finally
    if Context <> nil then CGContextRelease(Context);
    if ColorSpace <> nil then CGColorSpaceRelease(ColorSpace);
    Dest.Unmap(MapRec);
  end;
end;

function CreateCGImageFromBitmap(const ABitmap: TBitmap): CGImageRef;
var
  Context: CGContextRef;
  ColorSpace: CGColorSpaceRef;
  MapRec: TBitmapData;
begin
  ColorSpace := nil;
  Context := nil;
  if not ABitmap.Map(TMapAccess.maRead, MapRec) then
    raise EInvalidOperation.CreateRes(@SCannotMapBitmap);
  try
    ColorSpace := CGColorSpaceCreateDeviceRGB;
    Context := CGBitmapContextCreate(MapRec.Data, ABitmap.Width, ABitmap.Height, 8,
      MapRec.Pitch, ColorSpace, kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big);
    Result := CGBitmapContextCreateImage(Context);
  finally
    if Context <> nil then CGContextRelease(Context);
    if ColorSpace <> nil then CGColorSpaceRelease(ColorSpace);
    ABitmap.Unmap(MapRec);
  end;
end;

function FormatToNSString(const AFormat: TClipboardFormat): NSString; inline;
begin
  Result := TNSString.Wrap(CFStringRef(AFormat));
end;

function FormatsToNSArray(const AFormats: array of TClipboardFormat): NSArray;
begin
  Result := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@AFormats[0], Length(AFormats)))
end;

function NSDataToBytes(const Data: NSData): TBytes;
begin
  if Data = nil then Exit(nil);
  SetLength(Result, Data.length);
  Data.getBytes(Result, Length(Result));
end;

function NSStringGetValue(const NSStr: NSString): string;
begin
  if NSStr = nil then
    Result := ''
  else
    Result := CFStringGetValue((NSStr as ILocalObject).GetObjectID);
end;

{ TAppleClipboard }

class constructor TAppleClipboard.Create;
begin
  MaxBitmapSize := {$IFDEF IOS}1024{$ELSE}MaxInt{$ENDIF};
  FRegisteredFormats := TDictionary<string, TClipboardFormat>.Create;
end;

class destructor TAppleClipboard.Destroy;
var
  Format: TClipboardFormat;
begin
  for Format in FRegisteredFormats.Values do
    CFRelease(CFStringRef(Format));
  FRegisteredFormats.Free;
end;

function TAppleClipboard.GetFormatName(const AFormat: TClipboardFormat): string;
begin
  Result := CFStringGetValue(CFStringRef(AFormat));
end;

class function TAppleClipboard.RegisterStdFormat(const StdFormat: NSString): TClipboardFormat;
var
  CF: CFStringRef;
begin
  CF := (StdFormat as ILocalObject).GetObjectID;
  Result := TClipboardFormat(CF);
  CFRetain(CF);
  FRegisteredFormats.Add(CFStringGetValue(CF), Result);
end;

function TAppleClipboard.RegisterFormat(const AName: string): TClipboardFormat;
begin
  if FRegisteredFormats.TryGetValue(AName, Result) then Exit;
  Result := TClipboardFormat(CFStringCreateWithCharacters(nil, PChar(AName), Length(AName)));
  FRegisteredFormats.Add(AName, Result);
end;

class function TAppleClipboard.SameFormat(const Format1, Format2: TClipboardFormat): Boolean;
begin
  Result := (CFStringCompare(CFStringRef(Format1), CFStringRef(Format2), 0) = kCFCompareEqualTo);
end;

function TAppleClipboard.TypesToFormats(const Types: NSArray): TArray<TClipboardFormat>;
var
  I: Integer;
begin
  if Types = nil then Exit(nil);
  SetLength(Result, Types.count);
  for I := 0 to High(Result) do
    Result[I] := RegisterFormat(CFStringGetValue(Types.objectAtIndex(I)));
end;
{$ENDIF}
end.

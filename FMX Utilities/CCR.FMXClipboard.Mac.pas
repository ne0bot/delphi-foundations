{**************************************************************************************}
{                                                                                      }
{ FMX Utilities: TClipboard (OS X backend)                                             }
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

unit CCR.FMXClipboard.Mac;
{
  Part of a FireMonkey TClipboard implementation for Windows and OS X. The relation of
  this unit to CCR.FMXClipboard.pas is akin to the relation between FMX.Platform.Win.pas
  to FMX.Platform.pas.

  History
  - 29/7/13: if a PNG or TIFF is not available, get a bitmap via NSImage. This covers
    cases such as when only a PDF is on the clipboard.
  - 12/8/12: allow instantiation with a specific NSPasteboard.
}
interface

{$IFDEF MACOS}
uses
  Macapi.CoreFoundation, Macapi.Foundation, Macapi.AppKit, System.SysUtils, System.Classes,
  System.Generics.Collections, FMX.Types, CCR.FMXClipboard;

type
  TMacClipboard = class(TClipboard)
  private class var
    FcfPDF: TClipboardFormat;
    FRegisteredFormats: TDictionary<string, TClipboardFormat>;
  strict private
    FCustomPasteboard: NSPasteboard;
    FToAdd: TDictionary<TClipboardFormat, TBytes>;
    constructor DoCreate(const ACustomPasteboard: NSPasteboard; Dummy: Integer = 0); reintroduce;
    function GetPasteboard: NSPasteboard; inline;
  strict protected
    constructor CreateForSingleton(out cfText, cfBitmap, cfPNG, cfTIFF: TClipboardFormat); override;
    procedure DoAssignBitmap(ABitmap: TBitmap); override;
    procedure DoAssignBytes(AFormat: TClipboardFormat; const ASource: TBytes); override;
    procedure DoGetBitmap(ABitmap: TBitmap); override;
    procedure DoClear; override;
    function DoOpen: Boolean; override;
    procedure DoClose; override;
    function DoGetAsText: string; override;
    procedure DoSetAsText(const Value: string); override;
    function DoToBytes(AFormat: TClipboardFormat): TBytes; override;
  public
    constructor CreateForPasteboard(const APasteBoard: NSPasteboard);
    destructor Destroy; override;
    function GetFormats: TArray<TClipboardFormat>; override;
    function HasFormat(AFormat: TClipboardFormat): Boolean; override;
    function HasFormat(const AFormats: array of TClipboardFormat; out Matched: TClipboardFormat): Boolean; override;
    class function GetFormatName(AFormat: TClipboardFormat): string; override;
    class function RegisterFormat(const AName: string): TClipboardFormat; override;
    property Pasteboard: NSPasteboard read GetPasteboard;
  end;

function cfPDF: TClipboardFormat; inline;
{$ENDIF}

implementation

{$IFDEF MACOS}
uses
  System.RTLConsts, Macapi.CoreGraphics, Macapi.ObjCRuntime, Macapi.ObjectiveC, Macapi.CocoaTypes;

type
  //XE2's NSStringClass declaration is faulty, so redeclare with what we need
  NSStringClass = interface(NSObjectClass)
    ['{A21B9D92-0C1F-4BE4-9FA0-7E5357D46EDA}']
    function availableStringEncodings: PNSStringEncoding; cdecl;
    function defaultCStringEncoding: NSStringEncoding; cdecl;
    function localizedNameOfStringEncoding(encoding: NSStringEncoding): Pointer; cdecl;
    function localizedStringWithFormat(localizedStringWithFormat: NSString): Pointer; cdecl;
    function pathWithComponents(components: NSArray): Pointer; cdecl;
    function stringWithCString(bytes: PAnsiChar): Pointer; cdecl; overload;
    function stringWithCString(cString: PAnsiChar; encoding: NSStringEncoding): Pointer; cdecl; overload;
    function stringWithCharacters(characters: PChar; length: NSUInteger): Pointer; cdecl;
  end;
  TNSString = class(TOCGenericImport<NSStringClass, NSString>);

  TUserMemoryStream = class(TCustomMemoryStream)
    constructor Create(AMemory: Pointer; const ALength: NativeInt);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

constructor TUserMemoryStream.Create(AMemory: Pointer; const ALength: NativeInt);
begin
  inherited Create;
  SetPointer(AMemory, ALength);
end;

function TUserMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function CFStringCreate(const S: string): CFStringRef; inline;
begin
  Result := CFStringCreateWithCharacters(nil, PChar(S), Length(S));
end;

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

function FormatToNSString(const AFormat: TClipboardFormat): NSString; inline;
begin
  Result := TNSString.Wrap(CFStringRef(AFormat));
end;

function FormatsToNSArray(const AFormats: array of TClipboardFormat): NSArray;
begin
  Result := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@AFormats[0], Length(AFormats)))
end;

function IsFormat(const NS: NSString; const AFormat: TClipboardFormat): Boolean;
begin
  Result := (CFStringCompare((NS as ILocalObject).GetObjectID,
    CFStringRef(AFormat), 0) = kCFCompareEqualTo);
end;

function GetStdFormat(const NSStr: NSString): TClipboardFormat; inline;
begin
  Result := TClipboardFormat((NSStr as ILocalObject).GetObjectID);
end;

function cfPDF: TClipboardFormat;
begin
  Result := TMacClipboard.FcfPDF;
end;

{ TMacClipboard }

constructor TMacClipboard.DoCreate(const ACustomPasteboard: NSPasteboard; Dummy: Integer = 0);
begin
  inherited CreateForAnything;
  FCustomPasteboard := ACustomPasteboard;
  FToAdd := TDictionary<TClipboardFormat, TBytes>.Create;
  if ACustomPasteboard = nil then
    FRegisteredFormats := TDictionary<string, TClipboardFormat>.Create;
end;

constructor TMacClipboard.CreateForSingleton(out cfText, cfBitmap, cfPNG, cfTIFF: TClipboardFormat);
begin
  DoCreate(nil);
  cfText := GetStdFormat(NSPasteboardTypeString);
  cfPNG := GetStdFormat(NSPasteboardTypePNG);
  cfTIFF := GetStdFormat(NSPasteboardTypeTIFF);
  cfBitmap := cfPNG;
  FcfPDF := GetStdFormat(NSPasteboardTypePDF);
end;

constructor TMacClipboard.CreateForPasteboard(const APasteBoard: NSPasteboard);
begin
  if APasteBoard = nil then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  DoCreate(APasteBoard);
end;

destructor TMacClipboard.Destroy;
var
  Format: TClipboardFormat;
begin
  if FCustomPasteboard = nil then
  begin
    for Format in FRegisteredFormats.Values do
      CFRelease(CFStringRef(Format));
    FRegisteredFormats.Free;
  end;
  FToAdd.Free;
  inherited;
end;

procedure TMacClipboard.DoAssignBitmap(ABitmap: TBitmap);
begin
  Assign(cfPNG, ABitmap as IStreamPersist);
end;

procedure TMacClipboard.DoAssignBytes(AFormat: TClipboardFormat; const ASource: TBytes);
begin
  FToAdd.AddOrSetValue(AFormat, ASource);
end;

procedure TMacClipboard.DoSetAsText(const Value: string);
begin
  FToAdd.AddOrSetValue(cfText, WideBytesOf(Value));
end;

function TMacClipboard.DoToBytes(AFormat: TClipboardFormat): TBytes;
var
  Data: NSData;
begin
  Data := GetPasteboard.dataForType(FormatToNSString(AFormat));
  if Data = nil then Exit(nil);
  SetLength(Result, Data.length);
  Data.getBytes(Result, Length(Result));
end;

procedure TMacClipboard.DoGetBitmap(ABitmap: TBitmap);
var
  CFStr: CFStringRef;
  Objs: NSArray;
  Data: NSData;
  DataType: NSString;
  FileName: string;
  Pasteboard: NSPasteboard;
  Range: CFRange;
  Stream: TStream;
  URL: NSURL;
begin
  Pasteboard := GetPasteboard;
  //try pasting the file contents first (TextEdit does this)
  Objs := Pasteboard.readObjectsForClasses(TNSArray.Wrap(
    TNSArray.OCClass.arrayWithObject(objc_getClass('NSURL'))), nil);
  if (Objs <> nil) and (Objs.count > 0) then
  begin
    URL := TNSURL.Wrap(Objs.objectAtIndex(0));
    if URL.isFileURL then
    begin
      CFStr := (URL.path as ILocalObject).GetObjectID;
      Range.location := 0;
      Range.length := CFStringGetLength(CFStr);
      SetLength(FileName, Range.length);
      CFStringGetCharacters(CFStr, Range, PChar(FileName));
      if TryLoadBitmapFromFile(ABitmap, FileName) then Exit;
    end;
  end;
  //look for actual image data, initially the FMX 'native' formats, then via NSImage
  DataType := Pasteboard.availableTypeFromArray(FormatsToNSArray([cfPNG, cfTIFF]));
  if DataType <> nil then
    Data := Pasteboard.dataForType(DataType)
  else
  begin
    Objs := Pasteboard.readObjectsForClasses(TNSArray.Wrap(
      TNSArray.OCClass.arrayWithObject(objc_getClass('NSImage'))), nil);
    if (Objs <> nil) and (Objs.count > 0) then
      Data := TNSImage.Wrap(Objs.objectAtIndex(0)).TIFFRepresentation;
  end;
  if Data = nil then
  begin
    ABitmap.SetSize(0, 0);
    Exit;
  end;
  Stream := TUserMemoryStream.Create(Data.bytes, Data.length);
  try
    ABitmap.Clear(0); //needed due to FMX bug - try pasting the same PNG multiple times
    ABitmap.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMacClipboard.DoClear;
begin
  GetPasteboard.clearContents;
  FToAdd.Clear;
end;

procedure TMacClipboard.DoClose;
var
  Bytes: TBytes;
  Format: TClipboardFormat;
  Formats: TArray<TClipboardFormat>;
  Pasteboard: NSPasteboard;
begin
  Formats := FToAdd.Keys.ToArray;
  if Formats = nil then Exit;
  try
    Pasteboard := GetPasteboard;
    Pasteboard.declareTypes(FormatsToNSArray(Formats), nil);
    for Format in Formats do
    begin
      Bytes := FToAdd[Format];
      if Format = cfText then
        Pasteboard.setString(TNSString.Wrap(TNSString.OCClass.stringWithCharacters(
          PChar(Bytes), Length(Bytes) div 2)), NSPasteboardTypeString)
      else
        Pasteboard.setData(TNSData.Wrap(TNSData.OCClass.dataWithBytes(Bytes, Length(Bytes))),
          FormatToNSString(Format));
    end;
  finally
    FToAdd.Clear;
  end;
end;

function TMacClipboard.DoGetAsText: string;
begin
  Result := CFStringGetValue((GetPasteboard.stringForType(NSPasteboardTypeString) as ILocalObject).GetObjectID);
end;

function TMacClipboard.DoOpen: Boolean;
begin
  Result := True;
end;

function TMacClipboard.GetPasteboard: NSPasteboard;
begin
  if FCustomPasteboard = nil then
    Result := TNSPasteboard.Wrap(TNSPasteboard.OCClass.generalPasteboard)
  else
    Result := FCustomPasteboard;
end;

function TMacClipboard.GetFormats: TArray<TClipboardFormat>;
var
  I: Integer;
  Types: NSArray;
begin
  Types := GetPasteboard.types;
  SetLength(Result, Types.count);
  for I := 0 to High(Result) do
    Result[I] := RegisterFormat(CFStringGetValue(Types.objectAtIndex(I)));
end;

class function TMacClipboard.GetFormatName(AFormat: TClipboardFormat): string;
begin
  if AFormat = 0 then
    raise EArgumentException.CreateRes(@sArgumentInvalid);
  Result := CFStringGetValue(CFStringRef(AFormat));
end;

function TMacClipboard.HasFormat(AFormat: TClipboardFormat): Boolean;
var
  Dummy: TClipboardFormat;
begin
  if AFormat = cfBitmap then
    Result := TNSImage.OCClass.canInitWithPasteboard(Pasteboard)
  else
    Result := HasFormat([AFormat], Dummy);
end;

function TMacClipboard.HasFormat(const AFormats: array of TClipboardFormat;
  out Matched: TClipboardFormat): Boolean;
var
  NSArr: NSArray;
  NSStr: NSString;
begin
  NSArr := FormatsToNSArray(AFormats);
  NSStr := GetPasteboard.availableTypeFromArray(NSArr);
  Result := (NSStr <> nil);
  if Result then
    Matched := TClipboardFormat((NSStr as ILocalObject).GetObjectID)
  else
    Matched := 0;
end;

class function TMacClipboard.RegisterFormat(const AName: string): TClipboardFormat;
begin
  if FRegisteredFormats.TryGetValue(AName, Result) then Exit;
  Result := TClipboardFormat(CFStringCreateWithCharacters(nil, PChar(AName), Length(AName)));
  FRegisteredFormats.Add(AName, Result);
end;
{$ENDIF}

end.

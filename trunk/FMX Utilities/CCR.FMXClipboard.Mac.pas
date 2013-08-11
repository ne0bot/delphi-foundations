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
  Part of a FireMonkey TClipboard implementation. The relation of this unit to
  CCR.FMXClipboard.pas is akin to the relation between FMX.Platform.Mac.pas and
  FMX.Platform.pas. This unit supports XE2 or later.

  History
  - 11/8/13: further tweaks and parts split out into CCR.FMXClipboard.Apple.pas
    following introduction of CCR.FMXClipboard.iOS.pas.
  - 31/7/13: fiddled about with TClipboardFormat so that it isn't a number any more.
  - 29/7/13: if neither a PNG nor TIFF on the clipboard, other picture formats now
    supported by dint of using NSImage as an intermediary.
  - 12/8/12: allow instantiation with a specific NSPasteboard.
}
interface

{$IFDEF MACOS}
{$IFNDEF IOS}
uses
  Macapi.CocoaTypes, Macapi.Foundation, Macapi.AppKit,
  Macapi.ObjectiveC, Macapi.CoreFoundation, System.SysUtils, System.Classes,
  System.Generics.Collections, System.UITypes, FMX.Types, CCR.FMXClipboard, CCR.FMXClipboard.Apple;

type
  TMacClipboard = class(TAppleClipboard)
  private class var
    FcfPDF, FcfRTFD, FcfTabularText, FcfFont, FcfRuler, FcfColor, FcfSound,
    FcfMultipleTextSelection, FcfFindPanelSearchOptions: TClipboardFormat;
  strict private
    FCustomPasteboard: NSPasteboard;
    FToAdd: TDictionary<TClipboardFormat, TBytes>;
    constructor Create(const ACustomPasteboard: NSPasteboard; Dummy: Integer = 0); //avoid compiler warning re C++
    function GetPasteboard: NSPasteboard; inline;
  strict protected
    class function TryCreateSingleton(out Inst: TClipboard;
      out cfText, cfRTF, cfBitmap, cfPNG, cfTIFF: TClipboardFormat): Boolean; override;
    procedure DoAssignBitmap(const ABitmap: TBitmap); override;
    procedure DoAssignBytes(const AFormat: TClipboardFormat; const ASource: TBytes); override;
    function DoGetBitmap(ABitmap: TBitmap): Boolean; override;
    procedure DoClear; override;
    function DoOpen: Boolean; override;
    procedure DoClose; override;
    function DoGetAsText: string; override;
    procedure DoSetAsText(const Value: string); override;
    function DoToBytes(const AFormat: TClipboardFormat): TBytes; override;
  public
    constructor CreateForPasteboard(const APasteBoard: NSPasteboard);
    destructor Destroy; override;
    function GetFormats: TArray<TClipboardFormat>; override;
    function HasFormat(const AFormat: TClipboardFormat): Boolean; override;
    function HasFormat(const AFormats: array of TClipboardFormat; var Matched: TClipboardFormat): Boolean; override;
    property Pasteboard: NSPasteboard read GetPasteboard;
  end;

const
  PlatformClipboardClass: TClipboardClass = TMacClipboard;

function cfPDF: TClipboardFormat; inline;
function cfRTFD: TClipboardFormat; inline;
function cfTabularText: TClipboardFormat; inline;
function cfFont: TClipboardFormat; inline;
function cfRuler: TClipboardFormat; inline;
function cfColor: TClipboardFormat; inline;
function cfSound: TClipboardFormat; inline;
function cfMultipleTextSelection: TClipboardFormat; inline;
function cfFindPanelSearchOptions: TClipboardFormat; inline;
{$ENDIF}
{$ENDIF}

implementation

{$IFDEF MACOS}
{$IFNDEF IOS}
uses
  System.RTLConsts, Macapi.ObjCRuntime;

type
  {$IF NOT DECLARED(TIdleMessage)}
  //XE2 and XE3's NSStringClass declaration is faulty, so redeclare with what we need
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

  NSImage = interface(NSObject) //XE2's NSImage declaration was faulty, so same again
    ['{D39B2515-517A-4EA4-BA74-729198211680}']
    function CGImageForProposedRect(proposedDestRect: PNSRect; context: NSGraphicsContext; hints: NSDictionary): CGImageRef; cdecl;
  end;
  TNSImage = class(TOCGenericImport<NSImageClass, NSImage>)  end;
  {$IFEND}

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

function cfPDF: TClipboardFormat;
begin
  Result := TMacClipboard.FcfPDF;
end;

function cfRTFD: TClipboardFormat;
begin
  Result := TMacClipboard.FcfRTFD;
end;

function cfTabularText: TClipboardFormat;
begin
  Result := TMacClipboard.FcfTabularText;
end;

function cfFont: TClipboardFormat;
begin
  Result := TMacClipboard.FcfFont;
end;

function cfRuler: TClipboardFormat;
begin
  Result := TMacClipboard.FcfRuler;
end;

function cfColor: TClipboardFormat;
begin
  Result := TMacClipboard.FcfColor;
end;

function cfSound: TClipboardFormat;
begin
  Result := TMacClipboard.FcfSound;
end;

function cfMultipleTextSelection: TClipboardFormat;
begin
  Result := TMacClipboard.FcfMultipleTextSelection;
end;

function cfFindPanelSearchOptions: TClipboardFormat;
begin
  Result := TMacClipboard.FcfFindPanelSearchOptions;
end;

{ TMacClipboard }

constructor TMacClipboard.Create(const ACustomPasteboard: NSPasteboard; Dummy: Integer = 0);
begin
  inherited Create;
  FCustomPasteboard := ACustomPasteboard;
  if FCustomPasteboard <> nil then FCustomPasteboard.retain;
  FToAdd := TDictionary<TClipboardFormat, TBytes>.Create;
end;

class function TMacClipboard.TryCreateSingleton(out Inst: TClipboard;
  out cfText, cfRTF, cfBitmap, cfPNG, cfTIFF: TClipboardFormat): Boolean;
begin
  Inst := Create(nil);
  cfText := RegisterStdFormat(NSPasteboardTypeString);
  cfRTF := RegisterStdFormat(NSPasteboardTypeRTF);
  cfPNG := RegisterStdFormat(NSPasteboardTypePNG);
  cfTIFF := RegisterStdFormat(NSPasteboardTypeTIFF);
  cfBitmap := cfPNG;
  FcfPDF := RegisterStdFormat(NSPasteboardTypePDF);
  FcfRTFD := RegisterStdFormat(NSPasteboardTypeRTFD);
  FcfTabularText := RegisterStdFormat(NSPasteboardTypeTabularText);
  FcfFont := RegisterStdFormat(NSPasteboardTypeFont);
  FcfRuler := RegisterStdFormat(NSPasteboardTypeRuler);
  FcfColor := RegisterStdFormat(NSPasteboardTypeColor);
  FcfSound := RegisterStdFormat(NSPasteboardTypeSound);
  FcfMultipleTextSelection := RegisterStdFormat(NSPasteboardTypeMultipleTextSelection);
  FcfFindPanelSearchOptions := RegisterStdFormat(NSPasteboardTypeFindPanelSearchOptions);
  Result := True;
end;

constructor TMacClipboard.CreateForPasteboard(const APasteBoard: NSPasteboard);
begin
  if APasteBoard = nil then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  Create(APasteBoard);
end;

destructor TMacClipboard.Destroy;
begin
  if FCustomPasteboard <> nil then FCustomPasteboard.release;
  FToAdd.Free;
  inherited;
end;

procedure TMacClipboard.DoAssignBitmap(const ABitmap: TBitmap);
begin
  Assign(cfPNG, ABitmap as IStreamPersist);
end;

procedure TMacClipboard.DoAssignBytes(const AFormat: TClipboardFormat; const ASource: TBytes);
begin
  FToAdd.AddOrSetValue(AFormat, ASource);
end;

procedure TMacClipboard.DoSetAsText(const Value: string);
begin
  DoAssignBytes(cfText, WideBytesOf(Value));
end;

function TMacClipboard.DoToBytes(const AFormat: TClipboardFormat): TBytes;
begin
  Result := NSDataToBytes(GetPasteboard.dataForType(FormatToNSString(AFormat)));
end;

function TMacClipboard.DoGetBitmap(ABitmap: TBitmap): Boolean;
var
  CGRef: CGImageRef;
  Objs: NSArray;
  Data: NSData;
  DataType: NSString;
  Pasteboard: NSPasteboard;
  Stream: TStream;
  URL: NSURL;
begin
  Result := True;
  Pasteboard := GetPasteboard;
  //try pasting the file contents first (TextEdit does this)
  Objs := Pasteboard.readObjectsForClasses(TNSArray.Wrap(
    TNSArray.OCClass.arrayWithObject(objc_getClass('NSURL'))), nil);
  if (Objs <> nil) and (Objs.count > 0) then
  begin
    URL := TNSURL.Wrap(Objs.objectAtIndex(0));
    if URL.isFileURL and TryLoadBitmapFromFile(ABitmap, NSStringGetValue(URL.path)) then
      Exit;
  end;
  //look for actual image data, initially the FMX 'native' formats, then via NSImage
  DataType := Pasteboard.availableTypeFromArray(FormatsToNSArray([cfPNG, cfTIFF]));
  if DataType <> nil then
    Data := Pasteboard.dataForType(DataType)
  else
  begin
    Objs := Pasteboard.readObjectsForClasses(TNSArray.Wrap(
      TNSArray.OCClass.arrayWithObject(objc_getClass('NSImage'))), nil);
    //getting a CGImage then copying the bitmap bits should usually be more efficient than
    //calling TIFFRepresentation to get a NSData and streaming the TIFF data in
    if (Objs <> nil) and (Objs.count > 0) then //Data := TNSImage.Wrap(Objs.objectAtIndex(0)).TIFFRepresentation;
    begin
      CGRef := TNSImage.Wrap(Objs.objectAtIndex(0)).CGImageForProposedRect(nil, nil, nil);
      if CGRef <> nil then
      begin
        CopyCGImageToBitmap(CGRef, ABitmap);
        Exit;
      end;
    end;
  end;
  if Data = nil then Exit(False);
  Stream := TUserMemoryStream.Create(Data.bytes, Data.length);
  try
    ABitmap.Clear(TAlphaColors.Null); //needed due to FMX bug - try pasting the same PNG multiple times
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
  Result := NSStringGetValue(GetPasteboard.stringForType(NSPasteboardTypeString));
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
begin
  Result := TypesToFormats(GetPasteboard.types);
end;

function TMacClipboard.HasFormat(const AFormat: TClipboardFormat): Boolean;
var
  Dummy: TClipboardFormat;
begin
  if AFormat = cfBitmap then
    Result := TNSImage.OCClass.canInitWithPasteboard(Pasteboard)
  else
    Result := HasFormat([AFormat], Dummy);
end;

function TMacClipboard.HasFormat(const AFormats: array of TClipboardFormat;
  var Matched: TClipboardFormat): Boolean;
var
  NSArr: NSArray;
  NSStr: NSString;
begin
  NSArr := FormatsToNSArray(AFormats);
  NSStr := GetPasteboard.availableTypeFromArray(NSArr);
  Result := (NSStr <> nil);
  if Result then
    Matched := TClipboardFormat((NSStr as ILocalObject).GetObjectID)
end;
{$ENDIF}
{$ENDIF}

end.

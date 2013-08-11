{**************************************************************************************}
{                                                                                      }
{ FMX Utilities: TClipboard (iOS backend)                                              }
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

unit CCR.FMXClipboard.iOS;
{
  Part of a FireMonkey TClipboard implementation. The relation of this unit to
  CCR.FMXClipboard.pas is akin to the relation between FMX.Platform.iOS.pas and
  FMX.Platform.pas. This unit supports XE4 or later.

  History
  - 11/8/13: initial release of iOS support for XE4+.
}
interface

{$IFDEF IOS}
uses
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit,
  Macapi.ObjectiveC, Macapi.CoreFoundation, System.SysUtils, System.Classes,
  FMX.Types, CCR.FMXClipboard, CCR.FMXClipboard.Apple;

type
  UIPasteboard = interface(NSObject)
    ['{9B817EE3-BC7F-48BA-92E2-DA35D69F09CD}']
    function URL: NSURL; cdecl;
    function URLs: NSArray; cdecl;
    procedure addItems(items: NSArray); cdecl;
    function changeCount: NSInteger; cdecl;
    function color: UIColor; cdecl;
    function colors: NSArray; cdecl;
    function containsPasteboardTypes(pasteboardTypes: NSArray): Boolean; cdecl; overload;
    function containsPasteboardTypes(pasteboardTypes: NSArray; inItemSet: NSIndexSet): Boolean; cdecl; overload;
    function dataForPasteboardType(pasteboardType: NSString): NSData; cdecl; overload;
    function dataForPasteboardType(pasteboardType: NSString; inItemSet: NSIndexSet): NSArray; cdecl; overload;
    function image: UIImage; cdecl;
    function images: NSArray; cdecl;
    function isPersistent: Boolean; cdecl;
    function itemSetWithPasteboardTypes(pasteboardTypes: NSArray): NSIndexSet; cdecl;
    function items: NSArray; cdecl;
    function name: NSString; cdecl;
    function numberOfItems: NSInteger; cdecl;
    function pasteboardTypes: NSArray; cdecl;
    function pasteboardTypesForItemSet(itemSet: NSIndexSet): NSArray; cdecl;
    procedure setColor(color: UIColor); cdecl;
    procedure setColors(colors: NSArray); cdecl;
    procedure setData(data: NSData; forPasteboardType: NSString); cdecl;
    procedure setImage(image: UIImage); cdecl;
    procedure setImages(images: NSArray); cdecl;
    procedure setItems(items: NSArray); cdecl;
    procedure setPersistent(persistent: Boolean); cdecl;
    procedure setString(string_: NSString); cdecl;
    procedure setStrings(strings: NSArray); cdecl;
    procedure setURL(URL: NSURL); cdecl;
    procedure setURLs(URLs: NSArray); cdecl;
    procedure setValue(value: Pointer; forPasteboardType: NSString); cdecl;
    function strings: Pointer; cdecl;
    function valueForPasteboardType(pasteboardType: NSString): Pointer; cdecl;
    function valuesForPasteboardType(pasteboardType: NSString; inItemSet: NSIndexSet): NSArray; cdecl;
    function &string: NSString; cdecl; //!!!added
  end;
  TUIPasteboard = class(TOCGenericImport<UIPasteboardClass, UIPasteboard>)  end;

  TiOSClipboard = class(TAppleClipboard)
  private class var
    cfGibberish: TClipboardFormat;
  strict private
    FCustomPasteboard: UIPasteboard;
    FToAdd: NSMutableDictionary;
    constructor Create(const ACustomPasteboard: UIPasteboard);
    function GetPasteboard: UIPasteboard; inline;
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
    constructor CreateForPasteboard(const APasteBoard: UIPasteboard);
    destructor Destroy; override;
    function GetFormats: TArray<TClipboardFormat>; override;
    function HasFormat(const AFormat: TClipboardFormat): Boolean; override;
    function HasFormat(const AFormats: array of TClipboardFormat; var Matched: TClipboardFormat): Boolean; override;
    property Pasteboard: UIPasteboard read GetPasteboard;
  end;

const
  PlatformClipboardClass: TClipboardClass = TiOSClipboard;

function cfJpeg: TClipboardFormat;
{$ENDIF}

implementation

{$IFDEF IOS}
uses
  System.RTLConsts;

function cfJpeg: TClipboardFormat;
begin
  Result := cfBitmap;
end;

{ TiOSClipboard }

constructor TiOSClipboard.Create(const ACustomPasteboard: UIPasteboard);
begin
  inherited Create;
  FCustomPasteboard := ACustomPasteboard;
  if FCustomPasteboard <> nil then FCustomPasteboard.retain;
end;

destructor TiOSClipboard.Destroy;
begin
  if FCustomPasteboard <> nil then FCustomPasteboard.release;
  inherited;
end;

class function TiOSClipboard.TryCreateSingleton(out Inst: TClipboard;
  out cfText, cfRTF, cfBitmap, cfPNG, cfTIFF: TClipboardFormat): Boolean;
begin
  Inst := Create(nil);
  cfText := Inst.RegisterFormat('public.utf8-plain-text');
  cfRTF := Inst.RegisterFormat('public.rtf');
  cfPNG := Inst.RegisterFormat('public.png');
  cfTIFF := Inst.RegisterFormat('public.tiff');
  cfBitmap := Inst.RegisterFormat('public.jpeg'); //iOS 6 at least will privilege the JPEG format for images
  cfGibberish := Inst.RegisterFormat('private.null'); //!!!workaround for UIPasteboard not having a Clear method
  Result := True;
end;

constructor TiOSClipboard.CreateForPasteboard(const APasteBoard: UIPasteboard);
begin
  if APasteBoard = nil then
    raise EArgumentNilException.CreateRes(@SArgumentNil);
  Create(APasteBoard);
end;

procedure TiOSClipboard.DoAssignBitmap(const ABitmap: TBitmap);
begin
  Assign(cfPNG, ABitmap as IStreamPersist);
end;

procedure TiOSClipboard.DoAssignBytes(const AFormat: TClipboardFormat; const ASource: TBytes);
var
  Data: PNSData;
begin
  Data := TNSData.OCClass.dataWithBytes(ASource, Length(ASource));
  FToAdd.setValue(Data, FormatToNSString(AFormat));
end;

procedure TiOSClipboard.DoSetAsText(const Value: string);
begin
  FToAdd.setValue((NSSTR(Value) as ILocalObject).GetObjectID, FormatToNSString(cfText));
end;

function TiOSClipboard.DoToBytes(const AFormat: TClipboardFormat): TBytes;
begin
  Result := NSDataToBytes(GetPasteboard.dataForPasteboardType(FormatToNSString(AFormat)));
end;

function TiOSClipboard.DoGetBitmap(ABitmap: TBitmap): Boolean;
var
  CocoaImage: UIImage;
begin
  CocoaImage := GetPasteboard.image;
  Result := (CocoaImage <> nil);
  if Result then CopyCGImageToBitmap(CocoaImage.CGImage, ABitmap);
end;

procedure TiOSClipboard.DoClear;
begin
  GetPasteboard.setValue(CFSTR(''), FormatToNSString(cfGibberish));
  FToAdd.removeAllObjects;
end;

procedure TiOSClipboard.DoClose;
var
  Arr: NSArray;
begin
  if FToAdd.count > 0 then
  begin
    Arr := TNSArray.Wrap(TNSArray.OCClass.arrayWithObject((FToAdd as ILocalObject).GetObjectID));
    GetPasteboard.setItems(Arr);
  end;
  FToAdd.release;
  FToAdd := nil;
end;

function TiOSClipboard.DoGetAsText: string;
begin
  Result := NSStringGetValue(GetPasteboard.&string)
end;

function TiOSClipboard.DoOpen: Boolean;
begin
  FToAdd := TNSMutableDictionary.Wrap(TNSMutableDictionary.OCClass.dictionaryWithCapacity(4));
  FToAdd.retain;
  Result := True;
end;

function TiOSClipboard.GetPasteboard: UIPasteboard;
begin
  if FCustomPasteboard = nil then
    Result := TUIPasteboard.Wrap(TUIPasteboard.OCClass.generalPasteboard)
  else
    Result := FCustomPasteboard;
end;

function TiOSClipboard.GetFormats: TArray<TClipboardFormat>;
begin
  Result := TypesToFormats(GetPasteboard.pasteboardTypes);
end;

function TiOSClipboard.HasFormat(const AFormat: TClipboardFormat): Boolean;
begin
  if AFormat = cfBitmap then
    Result := GetPasteboard.containsPasteboardTypes(FormatsToNSArray([cfPng, cfJpeg]))
  else
    Result := GetPasteboard.containsPasteboardTypes(FormatsToNSArray([AFormat]))
end;

function TiOSClipboard.HasFormat(const AFormats: array of TClipboardFormat;
  var Matched: TClipboardFormat): Boolean;
var
  Possible: TClipboardFormat;
  NSArr: NSArray;
  I: NSUInteger;
  J: NativeInt;
begin
  NSArr := GetPasteboard.pasteboardTypes;
  for I := 0 to NSArr.count - 1 do
  begin
    Possible := TClipboardFormat(NSArr.objectAtIndex(I));
    for J := Low(AFormats) to High(AFormats) do
      if Possible = AFormats[J] then
      begin
        Matched := Possible;
        Exit(True);
      end;
  end;
  Result := False;
end;
{$ENDIF}

end.

{**************************************************************************************}
{                                                                                      }
{ FMX Utilities: TClipboard                                                            }
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

unit CCR.FMXClipboard;
{
  Abstracts from the Windows clipboard and Cocoa pasteboard API to provide an interface
  very close to the VCL TClipboard.

  Usage:
  - As with the VCL TClipboard, read and write Clipboard.AsText to get and set plain
    text, and call Clipboard.Assign(Bitmap) and Bitmap.Assign(Clipboard) to assign and
    retrieve a TBitmap respectively.
  - Test whether a format is on the clipboard by calling Clipboard.HasFormat. This takes
    a TClipboardFormat value (e.g. cfBitmap, cfText); on Windows these are
    interchangeable with CF_XXX values (so, cfBitmap is the same as CF_BITMAP), on other
    platforms TClipboardFormat is a special opaque type.
  - To get a list of formats currently held on the clipboard, call Clipboard.GetFormats.
  - To register a custom clipboard format, pass a textual identifier for it to
    Clipboard.RegisterFormat. On Apple platforms the string should be of the form
    'com.mycompany.mytype', though ultimately anything will be accepted.
  - To convert a valid TClipboardFormat value back to the textual identifer, call
    Clipboard.GetFormatName.
  - To add data with multiple formats, use a Open/Close block a la the VCL TClipboard:

    Clipboard.Open;
    try
      Clipboard.AsText := 'Some text';
      Clipboard.Assign(MyBitmap);
    finally
      Clipboard.Close;
    end;

  - Data for custom formats is put on by calling one of the Clipboard.Assign overloads;
    data for custom formats is read off via Clipboard.ToBytes or Clipboard.ToStream.

  Currently Windows and OS X for XE2+ and iOS for XE4+ are supported fully; for unknown
  future target platforms, a default text-only implementation is provided that delegates
  to IFMXClipboardService.
}
interface

{$IFDEF NEXTGEN}
{$LEGACYIFEND ON}
{$ENDIF}

uses
  System.SysUtils, System.Classes, FMX.Types;

type
  EClipboardException = class(Exception);

  { On Windows, TClipboardFormat hold CF_XXX values; on OS X, typecast CFStrings. }
{$IFDEF MSWINDOWS}
  TClipboardFormat = type Cardinal;
{$ELSE}
  {$DEFINE UsingOpaqueTClipboardFormat}
  TClipboardFormat = packed record
  strict private
    Data: Pointer;
  public
    class operator Equal(const Format1, Format2: TClipboardFormat): Boolean;
    class operator NotEqual(const Format1, Format2: TClipboardFormat): Boolean;
  end;
{$ENDIF}

  TClipboardClass = class of TClipboard;

  TClipboard = class(TPersistent)
  private
    FOpenCount: Integer;
    FEmptied: Boolean;
    class var FInstance: TClipboard;
    class var FcfText, FcfRTF, FcfBitmap, FcfPNG, FcfTIFF: TClipboardFormat;
    class constructor InitializeClass;
    class destructor FinalizeClass;
    procedure Adding;
    function GetAsRTF: string; inline;
    procedure SetAsRTF(const Value: string);
    function GetAsText: string;
    procedure SetAsText(const Value: string);
  strict protected
    class function TryCreateSingleton(out Inst: TClipboard;
      out cfText, cfRTF, cfBitmap, cfPNG, cfTIFF: TClipboardFormat): Boolean; virtual;
    procedure DoAssignBitmap(const ABitmap: TBitmap); virtual; abstract;
    function DoGetBitmap(ABitmap: TBitmap): Boolean; virtual; abstract;
    function DoOpen: Boolean; virtual; abstract;
    procedure DoClose; virtual; abstract;
    procedure DoClear; virtual; abstract;
    function DoGetAsText: string; virtual; abstract;
    procedure DoSetAsText(const Value: string); virtual; abstract;
    //one of the following MUST be overridden and NOT call the inherited impl
    procedure DoAssignBytes(const AFormat: TClipboardFormat; const ABytes: TBytes); virtual;
    procedure DoAssignBuffer(const AFormat: TClipboardFormat; const ABuffer; ASize: Integer); virtual;
    //one of the following MUST be overridden and NOT call the inherited impl
    function DoToBytes(const AFormat: TClipboardFormat): TBytes; virtual;
    function DoToStream(const AFormat: TClipboardFormat; AStream: TStream): Integer; virtual;
  protected
    constructor Create;
    procedure AssignTo(Dest: TPersistent); override;
    class function SameFormat(const Format1, Format2: TClipboardFormat): Boolean; virtual;
  public
    procedure Open;
    procedure Close;
    procedure Assign(Source: TPersistent); overload; override;
    procedure Assign(const AFormat: TClipboardFormat; const ASource: IStreamPersist); reintroduce; overload;
    procedure Assign(const AFormat: TClipboardFormat; const ASource: TStream; AMaxSize: Integer = MaxInt); reintroduce; overload;
    procedure Assign(const AFormat: TClipboardFormat; const AData: TBytes); reintroduce; overload;
    procedure Assign(const AFormat: TClipboardFormat; const ABuffer; ASize: Integer); reintroduce; overload;
    procedure Clear;
    function GetFormats: TArray<TClipboardFormat>; virtual; abstract;
    function HasFormat(const AFormat: TClipboardFormat): Boolean; overload; virtual;
    function HasFormat(const AFormats: array of TClipboardFormat; var Matched: TClipboardFormat): Boolean; overload; virtual; abstract;
    function GetFormatName(const AFormat: TClipboardFormat): string; virtual; abstract;
    function RegisterFormat(const AName: string): TClipboardFormat; virtual; abstract;
    function ToBytes(const AFormat: TClipboardFormat): TBytes;
    function ToStream(const AFormat: TClipboardFormat; AStream: TStream): Integer; //returns no. of bytes written
    function ToString: string; override;
    property AsBytes[const Format: TClipboardFormat]: TBytes read ToBytes write Assign;
    property AsRTF: string read GetAsRTF write SetAsRTF;
    property AsText: string read GetAsText write SetAsText;
  end;

{$IF NOT DECLARED(TMapAccess)}
  {$DEFINE TBitmapHelperNeeded}
  TPixelFormat = (pfUnknown, pfA8R8G8B8);

  TBitmapData = record
    Data: Pointer;
    Pitch: Integer;
    PixelFormat: TPixelFormat;
    Source: TBitmap;
    function GetScanline(Row: Integer): Pointer;
  end;

  TMapAccess = (maRead, maWrite, maReadWrite);

  TBitmapHelper = class helper for TBitmap
    function Map(const Access: TMapAccess; var Data: TBitmapData): Boolean;
    procedure Unmap(var Data: TBitmapData);
  end;
{$ELSEIF FireMonkeyVersion < 18}
  {$DEFINE TBitmapDataHelperNeeded}
  TBitmapDataHelper = record helper for TBitmapData
    function GetScanline(Row: Integer): Pointer;
  end;
{$IFEND}

function Clipboard: TClipboard; inline;

function cfText: TClipboardFormat; inline;
function cfBitmap: TClipboardFormat; inline;
function cfTIFF: TClipboardFormat; inline;
function cfPNG: TClipboardFormat; inline;
function cfRTF: TClipboardFormat; inline;

function TryLoadBitmapFromFile(Bitmap: TBitmap; const FileName: string): Boolean;

resourcestring
  SCannotMapBitmap = 'Map method of TBitmap failed';
  SCannotOpenClipboard = 'Cannot open clipboard: %s';
  SUnbalancedClose = 'Unbalanced Clipboard.Close call';
  SRTFExpected = 'Data is not valid RTF';

implementation

uses
  System.Math, System.RTLConsts, System.Generics.Collections, FMX.Platform,
  {$IF FireMonkeyVersion >= 18}
  FMX.Surfaces,
  {$IFEND}
  {$IF DEFINED(MSWINDOWS)}
  CCR.FMXClipboard.Win;
  {$ELSEIF DEFINED(IOS)}
  CCR.FMXClipboard.iOS;
  {$ELSEIF DEFINED(MACOS)}
  CCR.FMXClipboard.Mac;
//  {$ELSEIF DEFINED(ANDROID)}
//  CCR.FMXClipboard.Android;
  {$IFEND}

{$IF FireMonkeyVersion >= 18} //provide a default implementation using IFMXClipboardService for future target platforms
type
  TDefaultClipboard = class(TClipboard)
  strict protected
    FFormats: TDictionary<string, TClipboardFormat>;
    FNextFormatInt: NativeUInt;
    FService: IFMXClipboardService;
    class function TryCreateSingleton(out Inst: TClipboard;
      out cfText, cfRTF, cfBitmap, cfPNG, cfTIFF: TClipboardFormat): Boolean; override;
    procedure DoAssignBitmap(const ABitmap: TBitmap); override;
    function DoGetBitmap(ABitmap: TBitmap): Boolean; override;
    function DoGetAsText: string; override;
    procedure DoSetAsText(const Value: string); override;
    procedure DoAssignBytes(const AFormat: TClipboardFormat; const ABytes: TBytes); override;
    function DoToBytes(const AFormat: TClipboardFormat): TBytes; override;
    function DoOpen: Boolean; override;
    procedure DoClose; override;
    procedure DoClear; override;
  public
    constructor Create;
    destructor Destroy; override;
    function GetFormats: TArray<TClipboardFormat>; override;
    function HasFormat(const AFormats: array of TClipboardFormat; var Matched: TClipboardFormat): Boolean; override;
    function GetFormatName(const AFormat: TClipboardFormat): string; override;
    function RegisterFormat(const AName: string): TClipboardFormat; override;
  end;
{$IFEND}

{$IF NOT DECLARED(PlatformClipboardClass)}
const
  PlatformClipboardClass: TClipboardClass = TDefaultClipboard;
{$IFEND}

function Clipboard: TClipboard;
begin
  Result := TClipboard.FInstance;
end;

function cfText: TClipboardFormat;
begin
  Result := TClipboard.FcfText
end;

function cfBitmap: TClipboardFormat;
begin
  Result := TClipboard.FcfBitmap
end;

function cfTIFF: TClipboardFormat;
begin
  Result := TClipboard.FcfTIFF
end;

function cfPNG: TClipboardFormat;
begin
  Result := TClipboard.FcfPNG
end;

function cfRTF: TClipboardFormat;
begin
  Result := TClipboard.FcfRTF;
end;

function TryLoadBitmapFromFile(Bitmap: TBitmap; const FileName: string): Boolean;
{$IF FireMonkeyVersion < 17}
var
  Filter: TBitmapCodec;
begin
  Filter := DefaultBitmapCodecClass.Create;
  try
    Bitmap.Clear(0);
    if Filter.LoadFromFile(FileName, 0.0, Bitmap) then
    begin
      Bitmap.UpdateHandles;
      Bitmap.BitmapChanged;
      Exit(True);
    end;
  finally
    Filter.Free;
  end;
  Result := False;
end;
{$ELSEIF FireMonkeyVersion < 18}
begin
  Result := TBitmapCodecManager.LoadFromFile(FileName, Bitmap);
end;
{$ELSE}
var
  Surface: TBitmapSurface;
begin
  Surface := TBitmapSurface.Create;
  try
    Result := TBitmapCodecManager.LoadFromFile(FileName, Surface);
    if Result then Bitmap.Assign(Surface);
  finally
    Surface.Free;
  end;
end;
{$IFEND}

{ TClipboardFormat }

{$IFDEF UsingOpaqueTClipboardFormat}
class operator TClipboardFormat.Equal(const Format1, Format2: TClipboardFormat): Boolean;
begin
  Result := (Format1.Data = Format2.Data) or Clipboard.SameFormat(Format1, Format2);
end;

class operator TClipboardFormat.NotEqual(const Format1, Format2: TClipboardFormat): Boolean;
begin
  Result := not (Format1 = Format2);
end;
{$ENDIF}

{ TClipboard }

class constructor TClipboard.InitializeClass;
begin
  if PlatformClipboardClass.TryCreateSingleton(FInstance,
    FcfText, FcfRTF, FcfBitmap, FcfPng, FcfTIFF) then Exit;
  {$IF DECLARED(TDefaultClipboard)}
  if TDefaultClipboard.TryCreateSingleton(FInstance,
    FcfText, FcfRTF, FcfBitmap, FcfPng, FcfTIFF) then Exit;
  {$IFEND}
  Assert(False);
end;

class destructor TClipboard.FinalizeClass;
begin
  FInstance.Free;
end;

constructor TClipboard.Create;
begin
  inherited Create;
end;

class function TClipboard.TryCreateSingleton(out Inst: TClipboard;
  out cfText, cfRTF, cfBitmap, cfPNG, cfTIFF: TClipboardFormat): Boolean;
begin
  Result := False;
end;

procedure TClipboard.Open;
begin
  if FOpenCount = 0 then
  begin
    if not DoOpen then
      raise EClipboardException.CreateResFmt(@SCannotOpenClipboard, [SysErrorMessage(GetLastError)]);
    FEmptied := False;
  end;
  Inc(FOpenCount);
end;

procedure TClipboard.Clear;
begin
  Open;
  try
    DoClear;
  finally
    Close;
  end;
end;

procedure TClipboard.Close;
begin
  if FOpenCount = 0 then raise EClipboardException.CreateRes(@SUnbalancedClose);
  Dec(FOpenCount);
  if FOpenCount = 0 then DoClose;
end;

function TClipboard.GetAsRTF: string;
begin
  Result := StringOf(ToBytes(cfRTF))
end;

procedure TClipboard.SetAsRTF(const Value: string);
begin
  if Pos('\rtf', Value) = 0 then
    raise EArgumentException.CreateRes(@SRTFExpected);
  Assign(cfRTF, BytesOf(Value));
end;

function TClipboard.GetAsText: string;
begin
  Open;
  try
    Result := DoGetAsText;
  finally
    Close;
  end;
end;

procedure TClipboard.SetAsText(const Value: string);
begin
  Open;
  try
    Adding;
    DoSetAsText(Value);
  finally
    Close;
  end;
end;

procedure TClipboard.DoAssignBuffer(const AFormat: TClipboardFormat; const ABuffer; ASize: Integer);
var
  Bytes: TBytes;
begin
  SetLength(Bytes, ASize);
  if Bytes <> nil then Move(ABuffer, Bytes[0], ASize);
  DoAssignBytes(AFormat, Bytes);
end;

procedure TClipboard.DoAssignBytes(const AFormat: TClipboardFormat; const ABytes: TBytes);
begin
  DoAssignBuffer(AFormat, PByte(ABytes)^, Length(ABytes));
end;

function TClipboard.DoToBytes(const AFormat: TClipboardFormat): TBytes;
var
  Stream: TBytesStream;
begin
  Stream := TBytesStream.Create;
  try
    DoToStream(AFormat, Stream);
    Result := Stream.Bytes;
    SetLength(Result, Stream.Size);
  finally
    Stream.Free;
  end;
end;

function TClipboard.DoToStream(const AFormat: TClipboardFormat; AStream: TStream): Integer;
var
  Bytes: TBytes;
begin
  Bytes := DoToBytes(AFormat);
  Result := Length(Bytes);
  if Result > 0 then AStream.WriteBuffer(Bytes[0], Result);
end;

function TClipboard.ToBytes(const AFormat: TClipboardFormat): TBytes;
begin
  Open;
  try
    Result := DoToBytes(AFormat);
  finally
    Close;
  end;
end;

function TClipboard.ToStream(const AFormat: TClipboardFormat; AStream: TStream): Integer;
begin
  Open;
  try
    Result := DoToStream(AFormat, AStream);
  finally
    Close;
  end;
end;

function TClipboard.ToString: string;
begin
  Result := AsText;
end;

procedure TClipboard.Assign(const AFormat: TClipboardFormat; const AData: TBytes);
begin
  Open;
  try
    Adding;
    DoAssignBytes(AFormat, AData);
  finally
    Close;
  end;
end;

function TClipboard.HasFormat(const AFormat: TClipboardFormat): Boolean;
var
  Dummy: TClipboardFormat;
begin
  Result := HasFormat([AFormat], Dummy);
end;

procedure TClipboard.Assign(Source: TPersistent);
begin
  if Source = nil then
    Clear
  else if Source is TBitmap then
  begin
    Open;
    try
      Adding;
      DoAssignBitmap(TBitmap(Source))
    finally
      Close;
    end;
  end
  else if Source is TStrings then
    SetAsText(TStrings(Source).Text)
  else
    inherited;
end;

procedure TClipboard.Adding;
begin
  if (FOpenCount <> 0) and not FEmptied then
  begin
    Clear;
    FEmptied := True;
  end;
end;

procedure TClipboard.Assign(const AFormat: TClipboardFormat; const ASource: TStream; AMaxSize: Integer);
var
  Bytes: TBytes;
  Size: Integer;
begin
  Size := Min(ASource.Size - ASource.Position, AMaxSize);
  if Size = 0 then
    Bytes := nil
  else if (ASource is TBytesStream) and (ASource.Position = 0) then
  begin
    Bytes := TBytesStream(ASource).Bytes;
    SetLength(Bytes, Size);
    ASource.Seek(Size, soCurrent);
  end
  else
  begin
    SetLength(Bytes, Size);
    ASource.ReadBuffer(Bytes[0], Size);
  end;
  Assign(AFormat, Bytes);
end;

procedure TClipboard.Assign(const AFormat: TClipboardFormat; const ASource: IStreamPersist);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    if ASource <> nil then ASource.SaveToStream(Stream);
    Stream.Position := 0;
    Assign(AFormat, Stream);
  finally
    Stream.Free;
  end;
end;

procedure TClipboard.AssignTo(Dest: TPersistent);
begin
  if Dest is TBitmap then
  begin
    Open;
    try
      if not DoGetBitmap(TBitmap(Dest)) then
        TBitmap(Dest).SetSize(0, 0);
    finally
      Close;
    end;
  end
  else if Dest is TStrings then
    TStrings(Dest).Text := AsText
  else
    inherited;
end;

procedure TClipboard.Assign(const AFormat: TClipboardFormat; const ABuffer; ASize: Integer);
begin
  Open;
  try
    DoAssignBuffer(AFormat, ABuffer, ASize);
  finally
    Close;
  end;
end;

class function TClipboard.SameFormat(const Format1, Format2: TClipboardFormat): Boolean;
begin
  Result := (NativeUInt(Format1) = NativeUInt(Format2));
end;

{$IF DECLARED(TDefaultClipboard)}

{ TDefaultClipboard }

class function TDefaultClipboard.TryCreateSingleton(out Inst: TClipboard;
  out cfText, cfRTF, cfBitmap, cfPNG, cfTIFF: TClipboardFormat): Boolean;
begin
  Inst := Create;
  cfText := Inst.RegisterFormat('cfText');
  cfRTF := Inst.RegisterFormat('cfRTF');
  cfBitmap := Inst.RegisterFormat('cfBitmap');
  cfPNG := Inst.RegisterFormat('cfPNG');
  cfTIFF := Inst.RegisterFormat('cfTIFF');
  Result := True;
end;

constructor TDefaultClipboard.Create;
begin
  inherited Create;
  TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(FService));
  FFormats := TDictionary<string, TClipboardFormat>.Create;
  FNextFormatInt := 100;
end;

destructor TDefaultClipboard.Destroy;
begin
  FFormats.Free;
  inherited;
end;

function TDefaultClipboard.DoOpen: Boolean;
begin
  Result := (FService <> nil);
end;

procedure TDefaultClipboard.DoClose;
begin
end;

procedure TDefaultClipboard.DoClear;
begin
  if FService <> nil then FService.SetClipboard('');
end;

procedure TDefaultClipboard.DoAssignBitmap(const ABitmap: TBitmap);
begin
  raise EConvertError.CreateResFmt(@SAssignError, [ABitmap.ClassName, ClassName]);
end;

function TDefaultClipboard.DoGetBitmap(ABitmap: TBitmap): Boolean;
begin
  Result := False;
end;

function TDefaultClipboard.DoGetAsText: string;
begin
  if FService <> nil then
    Result := FService.GetClipboard.ToString
  else
    Result := '';
end;

procedure TDefaultClipboard.DoSetAsText(const Value: string);
begin
  if FService <> nil then
    FService.SetClipboard(Value);
end;

procedure TDefaultClipboard.DoAssignBytes(const AFormat: TClipboardFormat; const ABytes: TBytes);
var
  SourceName: string;
begin
  if AFormat = cfText then
    DoSetAsText(WideStringOf(ABytes))
  else
  begin
    SourceName := GetFormatName(AFormat);
    if SourceName = '' then SourceName := 'TBytes';
    raise EConvertError.CreateResFmt(@SAssignError, [SourceName, ClassName]);
  end;
end;

function TDefaultClipboard.DoToBytes(const AFormat: TClipboardFormat): TBytes;
begin
  if AFormat = cfText then
    Result := WideBytesOf(ToString)
  else
    Result := nil;
end;

function TDefaultClipboard.GetFormats: TArray<TClipboardFormat>;
begin
  if (FService <> nil) and not FService.GetClipboard.IsEmpty then
    Result := TArray<TClipboardFormat>.Create(cfText)
  else
    Result := nil;
end;

function TDefaultClipboard.HasFormat(const AFormats: array of TClipboardFormat; var Matched: TClipboardFormat): Boolean;
var
  Fmt: TClipboardFormat;
begin
  if FService <> nil then
    for Fmt in AFormats do
      if Fmt = cfText then
      begin
        Result := not FService.GetClipboard.IsEmpty;
        Exit;
      end;
  Result := False;
end;

function TDefaultClipboard.GetFormatName(const AFormat: TClipboardFormat): string;
var
  Pair: TPair<string,TClipboardFormat>;
begin
  for Pair in FFormats do
    if Pair.Value = AFormat then
      Exit(Pair.Key);
  Result := '';
end;

function TDefaultClipboard.RegisterFormat(const AName: string): TClipboardFormat;
begin
  if FFormats.TryGetValue(AName, Result) then Exit;
  Result := TClipboardFormat(FNextFormatInt);
  Inc(FNextFormatInt);
  FFormats.Add(AName, Result);
end;
{$IFEND}

{$IFDEF TBitmapHelperNeeded}
function TBitmapData.GetScanline(Row: Integer): Pointer;
begin
  Result := Source.ScanLine[Row];
end;

function TBitmapHelper.Map(const Access: TMapAccess; var Data: TBitmapData): Boolean;
begin
  Data.Data := StartLine;
  Data.Pitch := Width * 4;
  Data.PixelFormat := TPixelFormat.pfA8R8G8B8;
  Data.Source := Self;
  Result := True;
end;

procedure TBitmapHelper.Unmap(var Data: TBitmapData);
begin
  UpdateHandles;
  BitmapChanged;
end;
{$ENDIF}

{$IFDEF TBitmapDataHelperNeeded}
function TBitmapDataHelper.GetScanline(Row: Integer): Pointer;
begin
  Result := @PByte(Data)[Pitch * Row];
end;
{$ENDIF}

end.

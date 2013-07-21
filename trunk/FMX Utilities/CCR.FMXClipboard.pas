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
  Abstracts from the Windows clipboard and Cocoa pasteboard API to provide an interface very close
  to the VCL TClipboard.
}
interface

uses
  System.SysUtils, System.Classes, FMX.Types;

type
  EClipboardException = class(Exception);

  { On Windows, TClipboardFormat hold CF_XXX values; on OS X, typecast CFStrings. }
  TClipboardFormat = type {$IFDEF MSWINDOWS}Cardinal{$ELSE}NativeUInt{$ENDIF};

  TClipboard = class(TPersistent)
  private
    FOpenCount: Integer;
    FEmptied: Boolean;
    class var FInstance: TClipboard;
    class var FcfText, FcfBitmap, FcfTIFF: TClipboardFormat;
    class constructor InitializeClass;
    class destructor FinalizeClass;
    procedure Adding;
    function GetAsText: string;
    procedure SetAsText(const Value: string);
  strict protected
    constructor CreateForAnything; //just calls the inherited Create, which is hidden otherwise
    constructor CreateForSingleton(out cfText, cfBitmap, cfTIFF: TClipboardFormat); virtual;
    procedure DoAssignBitmap(ABitmap: TBitmap); virtual; abstract;
    procedure DoAssignBytes(AFormat: TClipboardFormat; const ABytes: TBytes); virtual;
    procedure DoAssignBuffer(AFormat: TClipboardFormat; const ABuffer; ASize: Integer); virtual;
    procedure DoGetBitmap(ABitmap: TBitmap); virtual; abstract;
    procedure DoOpen; virtual; abstract;
    procedure DoClose; virtual; abstract;
    procedure DoClear; virtual; abstract;
    function DoGetAsText: string; virtual; abstract;
    procedure DoSetAsText(const Value: string); virtual; abstract;
    //one of the following MUST be overridden and NOT call the inherited impl
    function DoToBytes(AFormat: TClipboardFormat): TBytes; virtual;
    function DoToStream(AFormat: TClipboardFormat; AStream: TStream): Integer; virtual;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    class procedure Create; static; deprecated 'FMX TClipboard cannot be explicitly instantiated';
    procedure Open;
    procedure Close;
    procedure Assign(Source: TPersistent); overload; override;
    procedure Assign(AFormat: TClipboardFormat; const ASource: IStreamPersist); reintroduce; overload;
    procedure Assign(AFormat: TClipboardFormat; ASource: TStream; AMaxSize: Integer = MaxInt); reintroduce; overload;
    procedure Assign(AFormat: TClipboardFormat; const AData: TBytes); reintroduce; overload;
    procedure Assign(AFormat: TClipboardFormat; const ABuffer; ASize: Integer); reintroduce; overload;
    procedure Clear;
    function GetFormats: TArray<TClipboardFormat>; virtual; abstract;
    function HasFormat(AFormat: TClipboardFormat): Boolean; overload; virtual;
    function HasFormat(const AFormats: array of TClipboardFormat; out Matched: TClipboardFormat): Boolean; overload; virtual; abstract;
    class function GetFormatName(AFormat: TClipboardFormat): string; virtual; abstract;
    class function RegisterFormat(const AName: string): TClipboardFormat; virtual; abstract;
    function ToBytes(AFormat: TClipboardFormat): TBytes;
    function ToStream(AFormat: TClipboardFormat; AStream: TStream): Integer; //returns no. of bytes written
    function ToString: string; override;
    property AsText: string read GetAsText write SetAsText;
  end;

  TClipboardClass = class of TClipboard;

function Clipboard: TClipboard; inline;

function cfText: TClipboardFormat; inline;
function cfBitmap: TClipboardFormat; inline;
function cfTIFF: TClipboardFormat; inline;

function TryLoadBitmapFromFile(Bitmap: TBitmap; const FileName: string): Boolean;

implementation

uses
  {$IFDEF MACOS}
  CCR.FMXClipboard.Mac,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  CCR.FMXClipboard.Win,
  {$ENDIF}
  {$IF FireMonkeyVersion >= 18}
  FMX.Surfaces,
  {$ENDIF}
  System.Math, System.RTLConsts;

resourcestring
  SUnbalancedClose = 'Unbalanced Clipboard.Close call';

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

function TryLoadBitmapFromFile(Bitmap: TBitmap; const FileName: string): Boolean;
{$IF FireMonkeyVersion = 16}     //XE2
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
{$ELSEIF FireMonkeyVersion = 17} //XE3
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

const
  ConcreteClass: TClipboardClass =
    {$IFDEF MSWINDOWS}
    TWinClipboard
    {$ELSE}
    TMacClipboard
    {$ENDIF};

{ TClipboard }

class constructor TClipboard.InitializeClass;
begin
  FInstance := ConcreteClass.CreateForSingleton(FcfText, FcfBitmap, FcfTIFF);
end;

class destructor TClipboard.FinalizeClass;
begin
  FInstance.Free;
end;

class procedure TClipboard.Create;
begin
  raise ENoConstructException.CreateResFmt(@SNoConstruct, [ClassName]);
end;

constructor TClipboard.CreateForAnything;
begin
  inherited Create;
end;

constructor TClipboard.CreateForSingleton(out cfText, cfBitmap, cfTIFF: TClipboardFormat);
begin
  CreateForAnything;
end;

procedure TClipboard.Open;
begin
  if FOpenCount = 0 then
  begin
    DoOpen;
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

procedure TClipboard.DoAssignBuffer(AFormat: TClipboardFormat; const ABuffer; ASize: Integer);
var
  Bytes: TBytes;
begin
  SetLength(Bytes, ASize);
  if Bytes <> nil then Move(ABuffer, Bytes[0], ASize);
  DoAssignBytes(AFormat, Bytes);
end;

procedure TClipboard.DoAssignBytes(AFormat: TClipboardFormat; const ABytes: TBytes);
begin
  DoAssignBuffer(AFormat, PByte(ABytes)^, Length(ABytes));
end;

function TClipboard.DoToBytes(AFormat: TClipboardFormat): TBytes;
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

function TClipboard.DoToStream(AFormat: TClipboardFormat; AStream: TStream): Integer;
var
  Bytes: TBytes;
begin
  Bytes := DoToBytes(AFormat);
  Result := Length(Bytes);
  if Result > 0 then AStream.WriteBuffer(Bytes[0], Result);
end;

function TClipboard.ToBytes(AFormat: TClipboardFormat): TBytes;
begin
  Open;
  try
    Result := DoToBytes(AFormat);
  finally
    Close;
  end;
end;

function TClipboard.ToStream(AFormat: TClipboardFormat; AStream: TStream): Integer;
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

procedure TClipboard.Assign(AFormat: TClipboardFormat; const AData: TBytes);
begin
  Open;
  try
    Adding;
    DoAssignBytes(AFormat, AData);
  finally
    Close;
  end;
end;

function TClipboard.HasFormat(AFormat: TClipboardFormat): Boolean;
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

procedure TClipboard.Assign(AFormat: TClipboardFormat; ASource: TStream; AMaxSize: Integer);
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

procedure TClipboard.Assign(AFormat: TClipboardFormat; const ASource: IStreamPersist);
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
      DoGetBitmap(TBitmap(Dest))
    finally
      Close;
    end;
  end
  else if Dest is TStrings then
    TStrings(Dest).Text := AsText
  else
    inherited;
end;

procedure TClipboard.Assign(AFormat: TClipboardFormat; const ABuffer; ASize: Integer);
begin
  Open;
  try
    DoAssignBuffer(AFormat, ABuffer, ASize);
  finally
    Close;
  end;
end;

end.

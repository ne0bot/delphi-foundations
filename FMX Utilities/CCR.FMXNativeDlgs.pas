{**************************************************************************************}
{                                                                                      }
{ FMX Utilities: Native message boxes                                                  }
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
{ Chris Rolliston are Copyright (C) 2012 Chris Rolliston. All Rights Reserved.         }
{                                                                                      }
{**************************************************************************************}

unit CCR.FMXNativeDlgs;
{
  ShowMessage/ShowMessageFmt/MessageDlg substitutes that delegate to the native API
  on both Windows and OS X. Note that this unit's MessageDlg implementation only
  supports the subset of the standard MessageDlg's functionality that is supported
  by the Windows API's MessageBox function.
}
interface

uses
  System.SysUtils, System.UITypes, FMX.Consts, FMX.Forms;

const
  mtWarning = TMsgDlgType.mtWarning;
  mtError = TMsgDlgType.mtError;
  mtInformation = TMsgDlgType.mtInformation;
  mtConfirmation = TMsgDlgType.mtConfirmation;
  mtCustom = TMsgDlgType.mtCustom;

  mbYes	= TMsgDlgBtn.mbYes;
  mbNo = TMsgDlgBtn.mbNo;
  mbOK = TMsgDlgBtn.mbOK;
  mbCancel = TMsgDlgBtn.mbCancel;
  mbAbort = TMsgDlgBtn.mbAbort;
  mbRetry = TMsgDlgBtn.mbRetry;
  mbIgnore = TMsgDlgBtn.mbIgnore;
  mbAll	= TMsgDlgBtn.mbAll;
  mbNoToAll	= TMsgDlgBtn.mbNoToAll;
  mbYesToAll = TMsgDlgBtn.mbYesToAll;
  mbHelp = TMsgDlgBtn.mbHelp;
  mbClose = TMsgDlgBtn.mbClose;

  mbYesNo = [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo];
  mbYesNoCancel = [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel];
  mbYesAllNoAllCancel = [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbYesToAll, TMsgDlgBtn.mbNo,
    TMsgDlgBtn.mbNoToAll, TMsgDlgBtn.mbCancel];
  mbOKCancel = [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbCancel];
  mbAbortRetryIgnore = [TMsgDlgBtn.mbAbort, TMsgDlgBtn.mbRetry, TMsgDlgBtn.mbIgnore];
  mbAbortIgnore = [TMsgDlgBtn.mbAbort, TMsgDlgBtn.mbIgnore];

procedure ShowMessage(const Msg: string); inline;
procedure ShowMessageFmt(const Msg: string; const Params: array of const);
function MessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
  HelpCtx: Longint = 0): TModalResult;

function GetDlgTypeCaption(DlgType: TMsgDlgType): string;

implementation

{$IFDEF MSWINDOWS}
uses
  Winapi.Windows;

function MessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
  HelpCtx: Longint): TModalResult;
var
  Flags: UINT;
begin
  if Buttons = [mbOK] then
    Flags := MB_OK
  else if Buttons = mbOKCancel then
    Flags := MB_OKCANCEL
  else if Buttons = mbAbortRetryIgnore then
    Flags := MB_ABORTRETRYIGNORE
  else if Buttons = mbYesNoCancel then
    Flags := MB_YESNOCANCEL
  else if Buttons = mbYesNo then
    Flags := MB_YESNO
  else if Buttons = [mbRetry, mbCancel] then
    Flags := MB_RETRYCANCEL
  else
    raise EArgumentOutOfRangeException.Create('Unsupported TMsgDlgBtns combination');
  case DlgType of
    mtWarning: Flags := Flags or MB_ICONEXCLAMATION;
    mtError: Flags := Flags or MB_ICONERROR;
    mtInformation: Flags := Flags or MB_ICONINFORMATION;
    mtConfirmation: Flags := Flags or MB_ICONQUESTION;
  end;
  Result := MessageBox(0, PChar(Msg), PChar(GetDlgTypeCaption(DlgType)), Flags or MB_TASKMODAL);
end;
{$ENDIF}

{$IFDEF MACOS}
uses
  Macapi.CoreFoundation;

function CFStringCreateNoCopy(const S: string): CFStringRef; inline;
begin
  Result := CFStringCreateWithCharactersNoCopy(nil, PChar(S), Length(S), kCFAllocatorNull);
end;

function MessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
  HelpCtx: Longint): TModalResult;
var
  Flags: CFOptionFlags;
  S: string;
  LHeading, LMessage, Btn1, Btn2, Btn3: CFStringRef;
  Response: CFOptionFlags;
begin
  Btn1 := nil; Btn2 := nil; Btn3 := nil;
  if Buttons = [mbOK] then
    //
  else if Buttons = mbOKCancel then
    Btn2 := CFStringCreateNoCopy(SMsgDlgCancel)
  else if Buttons = mbAbortRetryIgnore then
  begin
    Btn1 := CFStringCreateNoCopy(StringReplace(SMsgDlgAbort, '&', '', []));
    Btn2 := CFStringCreateNoCopy(StringReplace(SMsgDlgRetry, '&', '', []));
    Btn3 := CFStringCreateNoCopy(StringReplace(SMsgDlgIgnore, '&', '', []));
  end
  else if Buttons = mbYesNoCancel then
  begin
    Btn1 := CFStringCreateNoCopy(StringReplace(SMsgDlgYes, '&', '', []));
    Btn2 := CFStringCreateNoCopy(StringReplace(SMsgDlgNo, '&', '', []));
    Btn3 := CFStringCreateNoCopy(SMsgDlgCancel);
  end
  else if Buttons = mbYesNo then
  begin
    Btn1 := CFStringCreateNoCopy(StringReplace(SMsgDlgYes, '&', '', []));
    Btn2 := CFStringCreateNoCopy(StringReplace(SMsgDlgNo, '&', '', []));
  end
  else if Buttons = [mbRetry, mbCancel] then
  begin
    Btn1 := CFStringCreateNoCopy(StringReplace(SMsgDlgRetry, '&', '', []));
    Btn2 := CFStringCreateNoCopy(SMsgDlgCancel);
  end
  else
    raise EArgumentOutOfRangeException.Create('Unsupported TMsgDlgBtns combination');
  case DlgType of
    mtError: Flags := kCFUserNotificationStopAlertLevel;
    mtWarning: Flags := kCFUserNotificationCautionAlertLevel;
    mtInformation, mtConfirmation: Flags := kCFUserNotificationNoteAlertLevel;
  else Flags := kCFUserNotificationPlainAlertLevel;
  end;
  S := GetDlgTypeCaption(DlgType);
  LHeading := CFStringCreateNoCopy(S);
  LMessage := CFStringCreateNoCopy(Msg);
  try
    CFUserNotificationDisplayAlert(0, Flags, nil, nil, nil, LHeading, LMessage, Btn1, Btn2, Btn3, Response);
    Result := mrCancel;
    case Response of
      kCFUserNotificationDefaultResponse:
        if mbOK in Buttons then
          Result := mrOk
        else if mbYes in Buttons then
          Result := mrYes
        else if mbAbort in Buttons then
          Result := mrAbort
        else if mbRetry in Buttons then
          Result := mrRetry
        else
          Assert(False);
      kCFUserNotificationAlternateResponse:
        if mbNo in Buttons then Result := mrNo;
      kCFUserNotificationOtherResponse:
        if mbIgnore in Buttons then Result := mrIgnore;
    else Assert(False);
    end;
  finally
    CFRelease(LHeading);
    CFRelease(LMessage);
    if Btn1 <> nil then CFRelease(Btn1);
    if Btn2 <> nil then CFRelease(Btn2);
    if Btn3 <> nil then CFRelease(Btn3);
  end;
end;
{$ENDIF}

procedure ShowMessageFmt(const Msg: string; const Params: array of const);
begin
  ShowMessage(Format(Msg, Params));
end;

procedure ShowMessage(const Msg: string);
begin
  MessageDlg(Msg, mtCustom, [mbOK], 0);
end;

function GetDlgTypeCaption(DlgType: TMsgDlgType): string;
begin
  case DlgType of
    mtWarning: Result := SMsgDlgWarning;
    mtError: Result := SMsgDlgError;
    mtInformation: Result := SMsgDlgInformation;
    mtConfirmation: Result := SMsgDlgConfirm;
  else
    Result := Application.Title;
    if Result = '' then
      Result := ChangeFileExt(ExtractFileName(GetModuleName(0)), '');
  end;
end;

type
  THelper = class
    class procedure ShowException(Sender: TObject; ExceptObject: Exception);
  end;

class procedure THelper.ShowException(Sender: TObject; ExceptObject: Exception);
var
  Msg: string;
  SubException: Exception;
begin
  Msg := ExceptObject.Message;
  while True do
  begin
    SubException := ExceptObject.GetBaseException;
    if SubException <> ExceptObject then
    begin
      ExceptObject := SubException;
      if ExceptObject.Message <> '' then
        Msg := ExceptObject.Message;
    end       
    else
      Break;
  end;
  if (Msg <> '') and (Msg[Length(Msg)] > '.') then Msg := Msg + '.';
  MessageDlg(Msg, mtError, [mbOK]);
end;

initialization
  Application.OnException := THelper.ShowException;
end.

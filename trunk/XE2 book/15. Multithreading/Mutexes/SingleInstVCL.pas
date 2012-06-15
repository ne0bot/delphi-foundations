unit SingleInstVCL;
{
  Uses a TMutex to enforce single instance, and a bit of direct Windows API coding to
  activate the original instance when a second is attempted to be run.
}
interface

type
  TAttemptedReopenEvent = reference to procedure (const CmdLine: string);

var
  OnAttemptedReopen: TAttemptedReopenEvent;

function CanRun: Boolean;

implementation

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, System.SyncObjs, Vcl.Forms;

const
  AppIdent = 'MyMutexDemoApp';
  CM_GETMAINFORMHANDLE = WM_USER + 1;

var
  HiddenWnd: HWND;
  Mutex: TMutex;

function CanRun: Boolean;
begin
  Result := (Mutex <> nil);
end;

function HiddenWndProc(Handle: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  MainFormWnd: HWND;
  S: string;
  Struct: PCopyDataStruct;
begin
  case Msg of
    CM_GETMAINFORMHANDLE: Result := Application.MainFormHandle;
    WM_COPYDATA:
    begin
      { The originating process must activate the new one, otherwise the new one's
        attempt to activate itself will just lead it to flash in the taskbar. However,
        only the activated window's own process can de-minimise it if it was minimised. }
      MainFormWnd := Application.MainFormHandle;
      if MainFormWnd <> 0 then
        if IsIconic(MainFormWnd) then OpenIcon(MainFormWnd);
      Result := 1;
      if Assigned(OnAttemptedReopen) then
        try
          Struct := PCopyDataStruct(lParam);
          SetString(S, PChar(Struct.lpData), Struct.cbData div 2);
          OnAttemptedReopen(S);
        except
          Application.HandleException(ExceptObject);
        end;
    end
  else
    Result := DefWindowProc(Handle, Msg, wParam, lParam);
  end;
end;

procedure DoInitialization;
var
  Data: TCopyDataStruct;
begin
  Mutex := TMutex.Create(nil, False, AppIdent);
  if Mutex.WaitFor(0) = wrSignaled then
  begin
    { as we have acquired the mutex, create a hidden window that subsequent
      instances can send data to }
    HiddenWnd := AllocateHWnd(nil);
    SetWindowText(HiddenWnd, AppIdent);
    SetWindowLongPtr(HiddenWnd, GWL_WNDPROC, LONG_PTR(@HiddenWndProc));
    Exit;
  end;
  FreeAndNil(Mutex);
  { locate the other instance, activate it, and pass it our command line }
  HiddenWnd := FindWindow(nil, AppIdent);
  if HiddenWnd = 0 then Exit;
  SetForegroundWindow(SendMessage(HiddenWnd, CM_GETMAINFORMHANDLE, 0, 0));
  Data.cbData := StrLen(CmdLine) * SizeOf(Char);
  Data.lpData := CmdLine;
  SendMessage(HiddenWnd, WM_COPYDATA, 0, LPARAM(@Data));
  HiddenWnd := 0;
end;

initialization
  DoInitialization;
finalization
  Mutex.Free;
  if HiddenWnd <> 0 then
  begin
    SetWindowLongPtr(HiddenWnd, GWL_WNDPROC, LONG_PTR(@DefWindowProc));
    DeallocateHWnd(HiddenWnd);
  end;
end.

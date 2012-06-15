unit SemaphoreForm;
{
  Multiprocess semaphore demo: only three instances of the application are allowed
  to have their animation going at any one time.

  In terms of implementation, the semaphore is created, waited and released in a
  background thread. In order for the thread to terminate gracefully, a TEvent is
  used for the main thread to signal when the application is closing down. In the
  first instance, the worker thread therefore waits on both the semaphore *and*
  the event, either being able to wake it; if the semaphore does, then
  the animation is enabled and the thread now waits on the event alone, otherwise
  it just terminates.
}
interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.SyncObjs, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ShellAnimations;

type
  TfrmSemaphore = class(TForm)
    AnimatedCtrl: TAnimate;
    lblInfo: TLabel;
    btnSpawnNewInst: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSpawnNewInstClick(Sender: TObject);
  private
    FSemaphoreThread: TThread;
    FTerminateEvent: TEvent;
  end;

var
  frmSemaphore: TfrmSemaphore;

implementation

{$R *.dfm}

procedure TfrmSemaphore.FormCreate(Sender: TObject);
begin
  FTerminateEvent := TEvent.Create(nil, {ManualReset=}False, {InitialState=}False, '');
  FSemaphoreThread := TThread.CreateAnonymousThread(
    procedure
    var
      Objs: THandleObjectArray;
      Semaphore: TSemaphore;
      SignalledObj: THandleObject;
    begin
      { If the OS-level semaphore object doesn't already exist, it will be created using the InitialCount
        and MaxCount args (here, 3 and 3), otherwise it will be 'opened' and those args ignored. }
      Semaphore := TSemaphore.Create(nil, 3, 3, ExtractFileName(ParamStr(0)));
      try
        { THandleObject.WaitForMultiple is a simple wrapper for the WaitForMultipleObjects API function. }
        Objs := THandleObjectArray.Create(Semaphore, FTerminateEvent);
        if (THandleObject.WaitForMultiple(Objs, INFINITE, False, SignalledObj) <> wrSignaled) or
           (SignalledObj <> Semaphore) then Exit;
        try
          { While it's only a single property, TAnimate.Active is still a VCL property and so changes to
            it require synchronising or queuing. }
          TThread.Queue(TThread.CurrentThread,
            procedure
            begin
              AnimatedCtrl.Active := True;
            end);
          { Having acquired the semaphore and posted the property change, wait on the terminate event. }
          FTerminateEvent.WaitFor(INFINITE);
        finally
          Semaphore.Release;
        end;
      finally
        Semaphore.Free;
      end;
    end);
  FSemaphoreThread.FreeOnTerminate := False; //FreeOnTerminate isn't safe to use with WaitFor
  FSemaphoreThread.Start;
end;

procedure TfrmSemaphore.FormDestroy(Sender: TObject);
begin
  FTerminateEvent.SetEvent; //signal to the semaphore thread that it should quit
  FSemaphoreThread.WaitFor; //wait on it quit
  FSemaphoreThread.Free;    //free it!
end;

procedure TfrmSemaphore.btnSpawnNewInstClick(Sender: TObject);
begin
  WinExec(PAnsiChar(AnsiString(ParamStr(0))), SW_SHOWNORMAL);
end;

end.

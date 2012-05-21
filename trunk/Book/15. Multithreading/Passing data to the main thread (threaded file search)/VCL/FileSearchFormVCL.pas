unit FileSearchFormVCL;
{
  Demonstrates passing data back to the main thread with use of a TQueue and TTimer. A FireMonkey
  version of the demo - almost identical - is also in the repository.

  An ordinary TQueue is used to share data, with access to it synchronised as appropriate using
  TMonitor. The worker thread is however relieved of the need to remember to lock the queue by
  virtue of never accessing it directly - instead, the thread is passed a callback procedure to
  call, which then takes care of the locking duties.
}
interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, FileSearchThread;

type
  TfrmFileSearchVCL = class(TForm)
    panTop: TPanel;
    edtSearchFor: TEdit;
    btnSearch: TButton;
    btnStop: TButton;
    tmrCheckForBatch: TTimer;
    StatusBar: TStatusBar;
    lsbFoundFiles: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edtSearchForKeyPress(Sender: TObject; var Key: Char);
    procedure btnSearchClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure tmrCheckForBatchTimer(Sender: TObject);
    procedure lsbFoundFilesDblClick(Sender: TObject);
    procedure lsbFoundFilesKeyPress(Sender: TObject; var Key: Char);
  strict private
    FFoundFilesQueue: TQueue<TArray<string>>;
    FWorkerThread: TFileSearchThread;
    procedure RunSelectedFile;
    procedure WorkerThreadTerminate(Sender: TObject);
  end;

var
  frmFileSearchVCL: TfrmFileSearchVCL;

implementation

uses Winapi.Windows, Winapi.ShellApi; //for ShellExecute

{$R *.dfm}

procedure TfrmFileSearchVCL.FormCreate(Sender: TObject);
begin
  FFoundFilesQueue := TQueue<TArray<string>>.Create;
end;

procedure TfrmFileSearchVCL.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FWorkerThread);
  FFoundFilesQueue.Free;
end;

procedure TfrmFileSearchVCL.RunSelectedFile;
var
  Index: Integer;
begin
  Index := lsbFoundFiles.ItemIndex;
  if Index >= 0 then
    ShellExecute(Handle, nil, PChar(lsbFoundFiles.Items[Index]), nil, nil, SW_SHOWNORMAL)
end;

procedure TfrmFileSearchVCL.WorkerThreadTerminate(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  btnStop.Enabled := False;
  tmrCheckForBatch.Enabled := False;
  if Application.Terminated then Exit;
  if FWorkerThread = nil then //i.e., btnStop.Click got called, and therefore, FreeAndNil(FWorkerThread)
    FFoundFilesQueue.Clear
  else
  begin
    lsbFoundFiles.Items.BeginUpdate;
    try
      for I := 1 to FFoundFilesQueue.Count do
        for S in FFoundFilesQueue.Dequeue do
          lsbFoundFiles.Items.Add(S);
    finally
      lsbFoundFiles.Items.EndUpdate;
    end;
    StatusBar.SimpleText := Format('Completed search - found %d item(s)', [lsbFoundFiles.Count]);
  end;
  btnSearch.Enabled := True;
end;

procedure TfrmFileSearchVCL.edtSearchForKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) and btnSearch.Enabled then
  begin
    btnSearch.Click;
    Key := #0; //disable the standard 'beep'
  end;
end;

procedure TfrmFileSearchVCL.btnSearchClick(Sender: TObject);
begin
  btnSearch.Enabled := False;
  lsbFoundFiles.Items.Clear;
  StatusBar.SimpleText := 'Searching...';
  FWorkerThread := TFileSearchThread.Create(edtSearchFor.Text, tmrCheckForBatch.Interval,
    procedure (const ABatch: TEnumerable<string>) //This is what the worker thread calls to add a
    var                                           //batch of files to the queue, which the main
      Arr: TArray<string>;                        //thread then reads off when the timer fires.
    begin
      Arr := ABatch.ToArray;
      MonitorEnter(FFoundFilesQueue);
      try
        FFoundFilesQueue.Enqueue(Arr);
      finally
        MonitorExit(FFoundFilesQueue);
      end;
    end);
  FWorkerThread.OnTerminate := WorkerThreadTerminate;
  FWorkerThread.Start;
  tmrCheckForBatch.Enabled := True;
  btnStop.Enabled := True;
end;

procedure TfrmFileSearchVCL.btnStopClick(Sender: TObject);
begin
  FreeAndNil(FWorkerThread);
  StatusBar.SimpleText := 'Stopped!';
end;

procedure TfrmFileSearchVCL.lsbFoundFilesDblClick(Sender: TObject);
begin
  RunSelectedFile;
end;

procedure TfrmFileSearchVCL.lsbFoundFilesKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then RunSelectedFile;
end;

procedure TfrmFileSearchVCL.tmrCheckForBatchTimer(Sender: TObject);
var
  Batch: TArray<string>;
  S: string;
begin
  StatusBar.SimpleText := 'Searching ' + FWorkerThread.CurrentDirectory;
  if not MonitorTryEnter(FFoundFilesQueue) then Exit; //little point waiting
  try
    if FFoundFilesQueue.Count = 0 then Exit;
    Batch := FFoundFilesQueue.Dequeue;
  finally
    MonitorExit(FFoundFilesQueue);
  end;
  lsbFoundFiles.Items.BeginUpdate;
  try
    for S in Batch do
      lsbFoundFiles.Items.Add(S);
  finally
    lsbFoundFiles.Items.EndUpdate;
  end;
end;

end.

unit FileSearchFormFMX;
{
  FMX version of the threaded file search demo. The approach for the UI is slightly different to the
  VCL version since we use the FMX TGrid, which does not hold its own cell data. However, the code
  for the background thread is exactly the same (its unit is shared between the two projects).

  You could in principle do away with FNextBatch and the timer, and just copy file name batches into
  FFoundFiles directly. However, doing that would necessitate protecting almost every access to
  FFoundFiles with a monitor.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Generics.Collections,
  FMX.Types, FMX.Objects, FMX.Controls, FMX.Forms, FMX.Edit, FMX.Layouts, FMX.Grid, FileSearchThread;

type
  TfrmFileSearchFMX = class(TForm)
    lyoTop: TLayout;
    edtSearchFor: TEdit;
    btnSearch: TButton;
    btnStop: TButton;
    grdFoundFiles: TGrid;
    StatusBar: TStatusBar;
    tmrCheckForBatch: TTimer;
    lblStatus: TLabel;
    colFileName: TColumn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edtSearchForKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure btnSearchClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure tmrCheckForBatchTimer(Sender: TObject);
    procedure grdFoundFilesDblClick(Sender: TObject);
    procedure grdFoundFilesKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure grdFoundFilesGetValue(Sender: TObject; const Col, Row: Integer; var Value: Variant);
    procedure grdFoundFilesResize(Sender: TObject);
    procedure grdFoundFilesApplyStyleLookup(Sender: TObject);
  strict private
    FFoundFiles, FNextBatch: TList<string>;
    FWorkerThread: TFileSearchThread;
    procedure RunSelectedFile;
    procedure WorkerThreadTerminate(Sender: TObject);
  end;

var
  frmFileSearchFMX: TfrmFileSearchFMX;

implementation

{$IFDEF MSWINDOWS}
uses Winapi.Windows, Winapi.ShellApi; //for SW_SHOWNORMAL and ShellExecute respectively
{$ELSE}
uses Posix.Stdlib, System.StrUtils;   //for _system and ReplaceStr respectively
{$ENDIF}

{$R *.fmx}

procedure ShellOpen(const FileName: string);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, nil, PChar(FileName), nil, nil, SW_SHOWNORMAL)
{$ELSE}
  _system(PAnsiChar(UTF8String('open "' +
    ReplaceStr(ReplaceStr(FileName, '\', '\\'), '"', '\"') + '"')));
{$ENDIF};
end;

procedure TfrmFileSearchFMX.FormCreate(Sender: TObject);
begin
  edtSearchFor.Text := {$IFDEF MSWINDOWS}'C:\'{$ELSE}'/'{$ENDIF} + '*.rtf';
  FFoundFiles := TList<string>.Create;
  FNextBatch := TList<string>.Create;
end;

procedure TfrmFileSearchFMX.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FWorkerThread);
  FNextBatch.Free;
  FFoundFiles.Free;
end;

procedure TfrmFileSearchFMX.RunSelectedFile;
var
  Index: Integer;
begin
  Index := grdFoundFiles.Selected;
  if Index >= 0 then
    ShellOpen(FFoundFiles[Index]);
end;

procedure TfrmFileSearchFMX.WorkerThreadTerminate(Sender: TObject);
begin
  btnStop.Enabled := False;
  tmrCheckForBatch.Enabled := False;
  if Application.Terminated then Exit;
  if FWorkerThread <> nil then //if FWorkerThread is nil, then btnStop must have been clicked
  begin
    FFoundFiles.AddRange(FNextBatch);
    grdFoundFiles.RowCount := FFoundFiles.Count;
    lblStatus.Text := Format('Completed search - found %d item(s)', [FFoundFiles.Count]);
  end;
  FNextBatch.Clear;
  btnSearch.Enabled := True;
end;

procedure TfrmFileSearchFMX.edtSearchForKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if (Key = vkReturn) and btnSearch.Enabled then
  begin
    btnSearchClick(nil);
    Key := 0; //disable the standard 'beep'
  end;
end;

procedure TfrmFileSearchFMX.btnSearchClick(Sender: TObject);
begin
  btnSearch.Enabled := False;
  grdFoundFiles.RowCount := 0;
  FFoundFiles.Clear;
  lblStatus.Text := 'Searching...';
  FWorkerThread := TFileSearchThread.Create(edtSearchFor.Text, tmrCheckForBatch.Interval,
    procedure (const ABatch: TEnumerable<string>) //This is what the worker thread calls to notify
    begin                                         //us of a batch of files found, which we (the main
      TMonitor.Enter(FNextBatch);                 //thread) then put into the main list when the
      try                                         //timer event executes.
        FNextBatch.AddRange(ABatch);
      finally
        TMonitor.Exit(FNextBatch);
      end;
    end);
  FWorkerThread.OnTerminate := WorkerThreadTerminate;
  FWorkerThread.Start;
  tmrCheckForBatch.Enabled := True;
  btnStop.Enabled := True;
end;

procedure TfrmFileSearchFMX.btnStopClick(Sender: TObject);
begin
  FreeAndNil(FWorkerThread);
  lblStatus.Text := 'Stopped!';
end;

procedure TfrmFileSearchFMX.tmrCheckForBatchTimer(Sender: TObject);
begin
  lblStatus.Text := 'Searching ' + FWorkerThread.CurrentDirectory;
  if not TMonitor.TryEnter(FNextBatch) then Exit; //little point waiting
  try
    FFoundFiles.AddRange(FNextBatch);
    FNextBatch.Clear;
  finally
    TMonitor.Exit(FNextBatch);
  end;
  grdFoundFiles.RowCount := FFoundFiles.Count;
end;

procedure TfrmFileSearchFMX.grdFoundFilesApplyStyleLookup(Sender: TObject);
  function GetShapeResource(const AName: string): TShape;
  begin
    Result := grdFoundFiles.FindStyleResource(AName) as TShape;
  end;
begin
  GetShapeResource('background').Fill.Color := claWhite; //claWhitesmoke;
  GetShapeResource('focus').Opacity := 0;
end;

procedure TfrmFileSearchFMX.grdFoundFilesDblClick(Sender: TObject);
begin
  RunSelectedFile;
end;

procedure TfrmFileSearchFMX.grdFoundFilesGetValue(Sender: TObject; const Col, Row: Integer;
  var Value: Variant);
begin
  Value := FFoundFiles[Row];
end;

procedure TfrmFileSearchFMX.grdFoundFilesKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    RunSelectedFile;
    Key := 0;
  end;
end;

procedure TfrmFileSearchFMX.grdFoundFilesResize(Sender: TObject);
begin
  colFileName.Width := grdFoundFiles.Width - 18;
end;

end.

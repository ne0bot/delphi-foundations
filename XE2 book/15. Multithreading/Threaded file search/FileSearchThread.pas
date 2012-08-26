unit FileSearchThread;
{
  This unit implements the secondary thread that does the actual file searching in both the VCL and
  FireMonkey versions of the demo.

  Found file names are posted to the main thread in batches based on a timeout, for which TStopWatch
  is used.
}
interface

uses
  System.SysUtils, System.Classes, System.Diagnostics, System.Generics.Collections, System.SyncObjs;

type
  TPostBatchProc = reference to procedure (const ABatch: TEnumerable<string>);

  TFileSearchThread = class(TThread)
  strict private
    FBatchPeriodInMSecs: LongWord;
    FCurrentBatch: TList<string>;
    FCurrentDirectory: string;
    FCurrentDirectoryCS: TCriticalSection;
    FPostBatchProc: TPostBatchProc;
    FSearchPath, FSearchFileSpec: string;
    FStopwatch: TStopwatch;
    function GetCurrentDirectory: string;
    procedure SetCurrentDirectory(const Value: string);
    procedure PostBatch;
    procedure SearchDir(const Path: string);
  protected
    procedure Execute; override;
  public
    constructor Create(const ASearchSpec: string; ABatchPeriodInMSecs: LongWord;
      const APostBatchProc: TPostBatchProc);
    destructor Destroy; override;
    property CurrentDirectory: string read GetCurrentDirectory; //accesss protected by a critical section
  end;

implementation

constructor TFileSearchThread.Create(const ASearchSpec: string; ABatchPeriodInMSecs: LongWord;
  const APostBatchProc: TPostBatchProc);
begin
  inherited Create(True);
  {$IFDEF MSWINDOWS}
  Priority := tpHigher;
  {$ENDIF}
  FBatchPeriodInMSecs := ABatchPeriodInMSecs;
  FCurrentBatch := TList<string>.Create;
  FCurrentDirectoryCS := TCriticalSection.Create;
  FPostBatchProc := APostBatchProc;
  FSearchPath := ExtractFilePath(ASearchSpec);
  FSearchFileSpec := ExtractFileName(ASearchSpec);
end;

destructor TFileSearchThread.Destroy;
begin
  FCurrentBatch.Free;
  FCurrentDirectoryCS.Free;
  inherited;
end;

function TFileSearchThread.GetCurrentDirectory: string;
begin
  FCurrentDirectoryCS.Acquire;
  Result := FCurrentDirectory;
  FCurrentDirectoryCS.Release;
end;

procedure TFileSearchThread.SetCurrentDirectory(const Value: string);
begin
  FCurrentDirectoryCS.Acquire;
  FCurrentDirectory := Value;
  FCurrentDirectoryCS.Release;
end;

procedure TFileSearchThread.Execute;
begin
  FStopwatch := TStopwatch.StartNew;
  SearchDir(FSearchPath);
  if not Terminated then
    PostBatch;
end;

procedure TFileSearchThread.PostBatch;
begin
  FPostBatchProc(FCurrentBatch);
  FCurrentBatch.Clear;
end;

procedure TFileSearchThread.SearchDir(const Path: string);
const
  SAnyMask = {$IFDEF MSWINDOWS}'*.*'{$ELSE}'*'{$ENDIF};
var
  SearchRec: TSearchRec;
begin
  if Terminated then Exit;
  SetCurrentDirectory(ExcludeTrailingPathDelimiter(Path));
  //find files in the immediate directory...
  if FindFirst(Path + FSearchFileSpec, faAnyFile and not faDirectory, SearchRec) = 0 then
  try
    repeat
      FCurrentBatch.Add(Path + SearchRec.Name);
      if FStopwatch.ElapsedMilliseconds > FBatchPeriodInMSecs then
      begin
        PostBatch;
        FStopwatch.Reset;
        FStopwatch.Start;
      end;
    until Terminated or (FindNext(SearchRec) <> 0);
  finally
    FindClose(SearchRec);
  end;
  //find sub-directories and recurse
  if FindFirst(Path + SAnyMask, faDirectory, SearchRec) = 0 then
  try
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and (SearchRec.Attr and faSymLink = 0) then
        SearchDir(Path + SearchRec.Name + PathDelim);
    until Terminated or (FindNext(SearchRec) <> 0);
  finally
    FindClose(SearchRec);
  end;
end;

end.

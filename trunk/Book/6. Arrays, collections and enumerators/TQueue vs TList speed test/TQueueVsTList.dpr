program TQueueVsTList;
{
  Simple performance test comparing TQueue with TList when used in a queue-type fashion.
  On my computer, TQueue is well over 100 times quicker than TList here. Also compared
  is a custom linked list-based queue implementation, which is a bit quicker again (the
  standard TQueue uses an array under the hood, so will be a bit more memory efficient
  though).
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Diagnostics,
  System.Math,
  System.Generics.Collections,
  CCR.LinkedQueue in '..\Linked lists\CCR.LinkedQueue.pas';

const
  Iterations = 1000;

type
  TDatum = array[1..10024] of Byte; //10K of bytes

var
  DummyDatum: TDatum;

procedure TestList;
var
  I: Integer;
  List: TList<TDatum>;
begin
  List := TList<TDatum>.Create;
  try
    List.Capacity := Iterations;
    for I := 1 to Iterations do
      List.Add(DummyDatum);
    for I := 1 to Iterations do
    begin
      DummyDatum := List[0];
      List.Delete(0);
    end;
  finally
    List.Free;
  end;
end;

procedure TestQueue;
var
  I: Integer;
  Queue: TQueue<TDatum>;
begin
  Queue := TQueue<TDatum>.Create;
  try
    Queue.Capacity := Iterations;
    for I := 1 to Iterations do
      Queue.Enqueue(DummyDatum);
    for I := 1 to Iterations do
      DummyDatum := Queue.Dequeue;
  finally
    Queue.Free;
  end;
end;

procedure TestLinkedQueue;
var
  I: Integer;
  Queue: TLinkedQueue<TDatum>;
begin
  Queue := TLinkedQueue<TDatum>.Create;
  try
    for I := 1 to Iterations do
      Queue.Enqueue(DummyDatum);
    for I := 1 to Iterations do
      DummyDatum := Queue.Dequeue;
  finally
    Queue.Free;
  end;
end;

procedure DoTest(const Desc: string; const Proc: TProcedure; var Result: Double);
var
  Stopwatch: TStopwatch;
begin
  Write('Timing ' + Desc, '... ');
  Stopwatch := TStopwatch.StartNew;
  Proc;
  Stopwatch.Stop;
  Result := Stopwatch.Elapsed.TotalMilliseconds;
  WriteLn(Format('%.2nms', [Result]));
end;

type
  TTimingNum = 1..3;
const
  FmtStr = '%s average: %.2nms (standard deviation %.2nms)';
var
  I: TTimingNum;
  ListTimings, QueueTimings, LinkedQueueTimings: array[TTimingNum] of Double;
  Mean, StdDev: Double;
begin
  for I := Low(TTimingNum) to High(TTimingNum) do
  begin
    DoTest('TList', TestList, ListTimings[I]);
    DoTest('TQueue', TestQueue, QueueTimings[I]);
    DoTest('TLinkedQueue', TestLinkedQueue, LinkedQueueTimings[I]);
  end;
  WriteLn;
  MeanAndStdDev(ListTimings, Mean, StdDev);
  Writeln(Format(FmtStr, ['TList', Mean, StdDev]));
  MeanAndStdDev(QueueTimings, Mean, StdDev);
  Writeln(Format(FmtStr, ['TQueue', Mean, StdDev]));
  MeanAndStdDev(LinkedQueueTimings, Mean, StdDev);
  Writeln(Format(FmtStr, ['TLinkedQueue', Mean, StdDev]));
  ReadLn;
end.

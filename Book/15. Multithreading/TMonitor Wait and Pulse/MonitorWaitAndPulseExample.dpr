program MonitorWaitAndPulseExample;
{
  Demonstration of TMonitor in a simple producer/consumer scenario: a producer thread gives raw
  material to the factory floor, which itself acts as a producer for the sales team, so to speak.
  In all, two monitors are used, one to manage the data passed between the first and second threads,
  and another to manage the data passed between the second and third. However, since using TMonitor
  directly can be a bit tricky, a dedictaed interface type is used (see ProducerConsumerData.pas).
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.StrUtils,
  ProducerConsumerData in 'ProducerConsumerData.pas';

procedure SimulateDoingSomeProperWork; //simulate doing some work by just spinning for a bit
const
  MinIterations = $FFFFF;
begin
  TThread.SpinWait(Random(MaxInt - MinIterations) + MinIterations);
end;

procedure ProducerThread(const ADest: IThreadedData<string>);
const
  Strings: array[1..6] of string = ('siht', 'si', 'a', 'omed', 'fo', 'rotinoMT');
var
  S: string;
begin
  for S in Strings do
  begin
    SimulateDoingSomeProperWork;
    ADest.Write(S);
  end;
  ADest.Write(''); //an empty string will denote the end
end;

procedure FirstConsumerThread(const ASource, ADest: IThreadedData<string>);
var
  S: string;
begin
  repeat
    S := ASource.Read;
    if S = '' then
    begin
      ADest.Write('');
      Exit;
    end;
    SimulateDoingSomeProperWork;
    ADest.Write(ReverseString(S));
  until False;
end;

procedure SecondConsumerThread(const ASource: IThreadedData<string>);
var
  S: string;
begin
  repeat
    S := ASource.Read;
    if S = '' then Exit;
    SimulateDoingSomeProperWork;
    Write(S + ' ');
  until False;
end;

var
  RawMaterial, FinishedProduct: IThreadedData<string>;
  ThreadCounter: TCountdownEvent;
begin
  Randomize;
  RawMaterial := TThreadedData<string>.Create;
  FinishedProduct := TThreadedData<string>.Create;
  ThreadCounter := TCountdownEvent.Create(3);
  WriteLn('Working...' + SLineBreak);
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        ProducerThread(RawMaterial);
      finally
        ThreadCounter.Signal;
      end;
    end).Start;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        FirstConsumerThread(RawMaterial, FinishedProduct);
      finally
        ThreadCounter.Signal;
      end;
    end).Start;
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        SecondConsumerThread(FinishedProduct);
      finally
        ThreadCounter.Signal;
      end;
    end).Start;
  ThreadCounter.WaitFor;
  WriteLn(SLineBreak);
  Write('Press ENTER to exit...');
  ReadLn;
end.

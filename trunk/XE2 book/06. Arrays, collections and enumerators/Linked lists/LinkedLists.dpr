program LinkedLists;
{
  Simple benchmarking of the relative performance of a stock TList and a TLinkedList
  implementation. For each of an integer, string and large custom value type, a list is
  instantiated and added to a number times, half to the end of the list and half to the
  bottom. The stock list's Capacity property is set up front to make things fair.

  Well, I say 'fair' - the whole test is still designed to show the linked list in its
  best light!
}
{$APPTYPE CONSOLE}
{$O+}

uses
  System.SysUtils,
  System.Diagnostics,
  System.Math,
  System.Generics.Collections,
  CCR.LinkedList in 'CCR.LinkedList.pas';

const
  Iterations = 40000;
  RunsPerTest = 3;
{
  Define a common interface for the two list classes to share. This does unfortunately
  require defining descendant classes to implement the interface however.
}
type
  IList<T> = interface
    function Add(const AValue: T): Integer;
    procedure Insert(Index: Integer; const Value: T);
  end;

  TInterfacedList<T> = class(TList<T>, IInterface, IList<T>)
    RefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    constructor Create(ACapacity: Integer); overload;
  end;

  TInterfacedLinkedList<T> = class(TLinkedList<T>, IInterface, IList<T>)
    RefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Add(const AValue: T): Integer;
    procedure Insert(Index: Integer; const Value: T);
  end;

  TTest<T> = record
    class function Run(const List: IList<T>; const Value: T): Int64; static;
    class procedure RunPair(const Value: T; out ArrayListResult,
      LinkedListResult: Double); static;
  end;

{ TInterfacedList<T> }

constructor TInterfacedList<T>.Create(ACapacity: Integer);
begin
  inherited Create;
  Capacity := ACapacity;
end;

function TInterfacedList<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := S_OK else Result := E_NOINTERFACE;
end;

function TInterfacedList<T>._AddRef: Integer;
begin
  Inc(RefCount);
  Result := RefCount;
end;

function TInterfacedList<T>._Release: Integer;
begin
  Dec(RefCount);
  Result := RefCount;
  if RefCount = 0 then Free;
end;

{ TInterfacedLinkedList<T> }

function TInterfacedLinkedList<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then Result := S_OK else Result := E_NOINTERFACE;
end;

function TInterfacedLinkedList<T>._AddRef: Integer;
begin
  Inc(RefCount);
  Result := RefCount;
end;

function TInterfacedLinkedList<T>._Release: Integer;
begin
  Dec(RefCount);
  Result := RefCount;
  if RefCount = 0 then Free;
end;

function TInterfacedLinkedList<T>.Add(const AValue: T): Integer;
begin
  Result := Count;
  inherited Add(AValue);
end;

procedure TInterfacedLinkedList<T>.Insert(Index: Integer; const Value: T);
begin
  inherited Insert(Index, Value);
end;

{ TTest<T> }

class function TTest<T>.Run(const List: IList<T>; const Value: T): Int64;
var
  Max, I: Integer;
  Stopwatch: TStopwatch;
begin
  //scale the number of iterations in the case of a big element type
  if SizeOf(T) > 100 then
    Max := Round(Iterations * 100 / SizeOf(T))
  else
    Max := Iterations;
  Stopwatch := TStopwatch.StartNew;
  for I := 1 to Max do
  begin
    List.Add(Value);
    List.Insert(0, Value);
  end;
  Stopwatch.Stop;
  Result := Stopwatch.ElapsedMilliseconds;
end;

class procedure TTest<T>.RunPair(const Value: T; out ArrayListResult, LinkedListResult: Double);
var
  List: IList<T>;
begin
  List := TInterfacedList<T>.Create(Iterations * 2);
  ArrayListResult := Run(List, Value);
  List := TInterfacedLinkedList<T>.Create;
  LinkedListResult := Run(List, Value);
end;

procedure RunTests;
type
  TTestValueType = array[1..1024] of Byte;
var
  ArrayTimes, LinkedTimes: array[1..RunsPerTest] of Double;

  procedure ReportResults(const TypeName: string);
  var
    Mean, StdDev: Double;
  begin
    WriteLn('*** ', TypeName, ' ***');
    MeanAndStdDev(ArrayTimes, Mean, StdDev);
    WriteLn('Array list: mean = ', Mean:2:2, 'ms, standard deviation = ', StdDev:2:2, 'ms');
    MeanAndStdDev(LinkedTimes, Mean, StdDev);
    WriteLn('Linked list: mean = ', Mean:2:2, 'ms, standard deviation = ', StdDev:2:2, 'ms');
    WriteLn;
  end;
var
  I: Integer;
  Dummy: TTestValueType;
begin
  for I := 1 to RunsPerTest do
    TTest<Integer>.RunPair(999, ArrayTimes[I], LinkedTimes[I]);
  ReportResults('Integer');

  for I := 1 to RunsPerTest do
    TTest<string>.RunPair('Blah blah blah blah blah', ArrayTimes[I], LinkedTimes[I]);
  ReportResults('String');

  for I := 1 to RunsPerTest do
    TTest<TTestValueType>.RunPair(Dummy, ArrayTimes[I], LinkedTimes[I]);
  ReportResults('Big value type');
end;

begin
  RunTests;
  Write('Press ENTER to exit...');
  ReadLn;
end.

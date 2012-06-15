program Standard;
{
  In this demo, a custom class, TLinkedQueue, implements its own enumerator from
  scratch. As a micro-optimisation, a record rather than a class is used for the
  enumerator. This does mean you have to watch out for the fact the enumerator's fields
  won't be zero-initialised, as they would be for a class, but that's barely an issue
  given the enumerator code is so minimal.
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Generics.Collections,
  CCR.LinkedQueue in '..\Linked lists\CCR.LinkedQueue.pas';

var
  Queue: TLinkedQueue<string>;
  S: string;
begin
  Queue := TLinkedQueue<string>.Create;
  try
    Queue.Enqueue('First');
    Queue.Enqueue('Second');
    Queue.Enqueue('Third');
    Queue.Enqueue('Fourth');
    for S in Queue do
      WriteLn(S);
  finally
    Queue.Free;
  end;
  ReadLn;
end.

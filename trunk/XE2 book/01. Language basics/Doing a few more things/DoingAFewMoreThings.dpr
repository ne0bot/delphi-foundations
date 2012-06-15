program DoingAFewMoreThings;

uses
  Vcl.Dialogs;
{
  ReverseString - sub-routine that returns the input string reversed, e.g. if 'hello' is passed in,
  'olleh' is returned.
}
function ReverseString(const S: string): string;                  //A 'function' returns something.
var                                                               //Local variable block.
  I, Len: Integer;                                                //Local variables.
begin
  Len := Length(S);                                               //Assignment statement.
  SetLength(Result, Len);                                         //Procedure call.
  for I := 1 to Len do                                            //'For/to' loop.
    Result[Len - I + 1] := S[I];                                  //Update the function result.
end;

{ ShowInfo - small shortcut for MessageDlg. }

procedure ShowInfo(const S: string);                              //A 'procedure' returns nothing.
begin                                                             //MessageDlg takes a string, a
  MessageDlg(S, mtInformation, [mbOK], 0);                        //value from an 'enumeration',
end;                                                              //a 'set' and an integer.

const                                                             //Constant block.
  MaxBlankEntries = 2;                                            //'True constant' declared.
var                                                               //Variable block.
  Counter: Integer;                                               //Variable declaration.
  TextToRev, RevText: string;                                     //Two more of the same type.
begin
  Counter := 0;                                                   //Initialise the counter.
  while InputQuery('Example Program',                             //'While' loop, using a
    'Enter text to reverse:', TextToRev) do                       //function call to determine
  begin                                                           //whether to proceed.
    if TextToRev = '' then                                        //'If' test - nothing entered?
    begin
      Inc(Counter);                                               //Adds 1 to the current value.
      if Counter = MaxBlankEntries then                           //Another 'if' test.
      begin
        ShowInfo('You must have had enough!');
        Break;                                                    //Break out of the loop.
      end;
      Continue;                                                   //Continue to the next iteration.
    end;
    RevText := ReverseString(TextToRev);                          //Call our custom function.
    ShowInfo('"' + TextToRev +                                    //Procedure call with the
      '" becomes "' + RevText + '"');                             //argument composed in place.
    Counter := 0;                                                 //Reset value for next round.
    TextToRev := '';                                              //Ditto.
  end;
end.

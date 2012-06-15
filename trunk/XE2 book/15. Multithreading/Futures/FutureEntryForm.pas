unit FutureEntryForm;
{
  Futures example - testing the validity of URLs. Assuming the 'Use futures' check box is still
  ticked/crossed, when either the Add or Add Examples button is clicked, testing is done as a
  background task. If you dawdle for a bit before clicking Get Results, these tasks may have already
  finished, making the results dialog come up almost instantly. See BetterFutures.txt and the source
  for BetterFutures.pas for more information about the mechanics of this.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.SyncObjs,
  FMX.Types, FMX.Objects, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo,
  System.Generics.Collections, FMX.ListBox, FMX.Edit, BetterFutures, FutureResultsForm;

type
  TfrmPrimeNumberTest = class(TForm)
    Label1: TLabel;
    btnGetResults: TButton;
    edtURLToTest: TEdit;
    btnAddURL: TButton;
    lsbNumsToTest: TListBox;
    btnClear: TButton;
    btnClose: TButton;
    chkUseFutures: TCheckBox;
    btnAddExamples: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnGetResultsClick(Sender: TObject);
    procedure btnAddURLClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure edtURLToTestExit(Sender: TObject);
    procedure btnAddExamplesClick(Sender: TObject);
    procedure chkUseFuturesChange(Sender: TObject);
  strict private
    FRecs: TList<TURLTestRec>;
    procedure AddURLForTesting(const AURL: string);
  end;

var
  frmPrimeNumberTest: TfrmPrimeNumberTest;

implementation

uses IdIOHandlerSocket, IdStack, IdHTTP;

{$R *.fmx}

type
  TIdHttpEx = class(TIdHTTP)
    function PeerIP: string;
    procedure Head(const AURL: string; AAcceptAllResponseCodes: Boolean);
  end;

function TIdHttpEx.PeerIP: string;
begin
  if IOHandler is TIdIOHandlerSocket then
    Result := TIdIOHandlerSocket(IOHandler).Binding.PeerIP
  else
    Result := '';
end;

procedure TIdHttpEx.Head(const AURL: string; AAcceptAllResponseCodes: Boolean);
const
  RecognizedCodes: array[1..39] of SmallInt = (200, 201, 202, 203, 204, 205, 206, 300, 301, 302, 303,
    304, 305, 306, 307, 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414,
    415, 416, 417, 500, 501, 502, 503, 504, 505); //http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
begin
  if AAcceptAllResponseCodes then                                 //An alternative is to call the
    DoRequest(Id_HTTPMethodHead, AURL, nil, nil, RecognizedCodes) //inherited Head and trap
  else                                                            //EIdHTTPProtocolException.
    inherited Head(AURL);
end;

var
  BadHostIP: TFunc<string>;

{ Anonymous method 'cookie cutter' - creates a URL tester for a specific address. }

function CreateURLTesterFunc(const AURL: string): TFunc<Integer>;
begin
  Result := function : Integer
            var
              IdHttp: TIdHttpEx;
            begin
              IdHttp := TIdHttpEx.Create(nil);
              try
                IdHttp.Head(AURL, True);
                Result := IdHttp.ResponseCode;
                { Handle the case of bad hosts being redirected to a default page which, obviously,
                  *does* exist. The IP address for the default page is itself retrieved using a future.}
                if (Result = 200) and (IdHttp.PeerIP = BadHostIP) then
                  Result := 404;
              finally
                IdHttp.Free;
              end;
            end;
end;

{ General form stuff }

procedure TfrmPrimeNumberTest.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  FRecs := TList<TURLTestRec>.Create;
  Randomize;
  //retrieve the IP address for any default page via a future
  BadHostIP := TFuture<string>.Create(
    function : string
    var
      IdHTTP: TIdHttpEx;
    begin
      Result := '';
      IdHTTP := TIdHttpEx.Create(nil);
      try
        try
          IdHTTP.Head('http://www.thisdoesnotreallyexist.ok', True);
        except
          on EIdSocketError do Exit;  //Raised if not connected to internet.
        end;
        if IdHTTP.ResponseCode = 200 then Result := IdHTTP.PeerIP;
      finally
        IdHTTP.Free;
      end;
    end);
end;

procedure TfrmPrimeNumberTest.FormDestroy(Sender: TObject);
begin
  FRecs.Free;
end;

procedure TfrmPrimeNumberTest.AddURLForTesting(const AURL: string);
var
  Item: TListBoxItem;
  Rec: TURLTestRec;
begin
  Rec.URLToTest := AURL;
  if chkUseFutures.IsChecked then
    Rec.TestFunc := TFuture<Integer>.Create(CreateURLTesterFunc(AURL))
  else
    Rec.TestFunc := CreateURLTesterFunc(AURL);
  Item := TListBoxItem.Create(Self);
  Item.Text := AURL;
  Item.Parent := lsbNumsToTest;
  FRecs.Add(Rec);
end;

procedure TfrmPrimeNumberTest.btnAddExamplesClick(Sender: TObject);

  procedure AddIfNotAlready(const URL: string);
  begin
    if lsbNumsToTest.Items.IndexOf(URL) < 0 then
      AddURLForTesting(URL);
  end;
begin
  AddIfNotAlready('http://twitter.com/#!/search/realtime/%23Delphi');
  AddIfNotAlready('http://stackoverflow.com/questions/tagged/delphi');
  AddIfNotAlready('http://docwiki.embarcadero.com/VCL/en/FMX.Memo.TMemo');
  AddIfNotAlready('http://www.google.com');
  AddIfNotAlready('http://www.idontthink.so');
  AddIfNotAlready('http://www.embarcadero.com/delphi');
  AddIfNotAlready('http://www.guardian.co.uk');
  AddIfNotAlready('http://www1.skysports.com/football/');
  AddIfNotAlready('http://www1.skysports.com/soccer/');
end;

procedure TfrmPrimeNumberTest.btnAddURLClick(Sender: TObject);
begin
  if edtURLToTest.Text = '' then Exit;
  AddURLForTesting(edtURLToTest.Text);
  edtURLToTest.Text := '';
end;

procedure TfrmPrimeNumberTest.btnGetResultsClick(Sender: TObject);
begin
  ShowResults(FRecs);
end;

procedure TfrmPrimeNumberTest.chkUseFuturesChange(Sender: TObject);
var
  S: string;
  URLs: TArray<string>;
begin
  if not chkUseFutures.IsChecked then
  begin
    S := BadHostIP;         //ensure it has finished
    TFutureTasks.CancelAll; //cancel any others
  end;
  //rebuild FRecs
  FRecs.Clear;
  lsbNumsToTest.BeginUpdate;
  try
    URLs := lsbNumsToTest.Items.ToStringArray;
    lsbNumsToTest.Clear;
    for S in URLs do
      AddURLForTesting(S);
  finally
    lsbNumsToTest.EndUpdate;
  end;
end;

procedure TfrmPrimeNumberTest.edtURLToTestExit(Sender: TObject);
var
  S: string;
begin
  S := Trim(edtURLToTest.Text);
  if (Pos(':', S) = 0) and (S <> '') then
    edtURLToTest.Text := 'http://' + S
  else
    edtURLToTest.Text := S;
end;

procedure TfrmPrimeNumberTest.btnClearClick(Sender: TObject);
begin
  TFutureTasks.CancelAll;
  FRecs.Clear;
  lsbNumsToTest.Clear;
end;

procedure TfrmPrimeNumberTest.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.

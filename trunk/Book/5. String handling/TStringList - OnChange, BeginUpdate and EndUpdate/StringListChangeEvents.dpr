program StringListChangeEvents;
{
  Trivial demo of assigning OnChanging and OnChange handlers to a TStringList,
  and the impact of calling BeginUpdate and EndUpdate if you do. In the case of
  TStrings instances tied to a VCL control (e.g. the Lines property of a TMemo),
  calling BeginUpdate and EndUpdate is even more important, especially once the
  updates get numerous - using BeginUpdate/EndUpdate pairs in such a situation
  will frequently speed things up considerably.
}
{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.Classes;

type
  TTest = class
  strict private
    FList: TStringList;
    procedure ListChanging(Sender: TObject);
    procedure ListChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateWithoutBeginAndEndUpdate;
    procedure UpdateWithBeginAndEndUpdate;
  end;

constructor TTest.Create;
begin
  inherited Create;
  FList := TStringList.Create;
  FList.OnChanging := ListChanging;
  FList.OnChange := ListChanged;
end;

destructor TTest.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TTest.ListChanging(Sender: TObject);
begin
  WriteLn('List changing...');
end;

procedure TTest.ListChanged(Sender: TObject);
begin
  WriteLn('List changed');
end;

procedure TTest.UpdateWithoutBeginAndEndUpdate;
var
  I: Integer;
begin
  for I := 1 to 20 do
    FList.Add(IntToStr(I))
end;

procedure TTest.UpdateWithBeginAndEndUpdate;
begin
  FList.BeginUpdate;
  try
    UpdateWithoutBeginAndEndUpdate;
  finally
    FList.EndUpdate;
  end;
end;

var
  Test: TTest;
begin
  Test := TTest.Create;
  try
    WriteLn('Updates without a Begin/EndUpdate pair:');
    Test.UpdateWithoutBeginAndEndUpdate;
    WriteLn;
    WriteLn('Same updates wrapped in a Begin/EndUpdate pair:');
    Test.UpdateWithBeginAndEndUpdate;
  finally
    Test.Free;
  end;
  WriteLn;
  Write('Press ENTER to exit...');
  ReadLn;
end.

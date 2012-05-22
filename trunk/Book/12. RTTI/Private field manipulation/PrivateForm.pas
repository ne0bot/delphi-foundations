unit PrivateForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses System.Rtti;

type
  TCollectionAccess = class(TCollection);

procedure SortCollection(ACollection: TCollection);
var
  Context: TRttiContext;
  InstType: TRttiInstanceType;
  List: TList;
begin
  InstType := Context.GetType(ACollection.ClassType).AsInstance;
  List := InstType.GetField('FItems').GetValue(ACollection).AsType<TList>;
  List.SortList(
    function (Item1, Item2: Pointer): Integer
    begin
      Result := CompareText(TCollectionItem(Item1).DisplayName,
        TCollectionItem(Item2).DisplayName, loUserLocale);
    end);
  TCollectionAccess(ACollection).Changed;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SortCollection(StatusBar1.Panels);
end;

end.

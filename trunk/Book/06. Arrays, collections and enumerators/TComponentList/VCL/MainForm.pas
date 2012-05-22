unit MainForm;
{
  Simple demo showing (a) the point of TComponentList - i.e., the fact it geta notified
  when one of its items is destroyed, allowing it to remove that item from itself
  automatically - and (b) an implementation of the missing generic TComponentList.

  The form creates four lists - a standard TComponentList, a generic TList, a generic
  TObjectList, and a custom generic TComponentList - each that gets added to it a
  reference to the the btnTest control. When you run the program, the four buttons at
  the top report the current value of each list's Count property, which will all be 1
  initially. Click the bottom button, which is btnTest, and it will then destroy itself.
  Click the top buttons again, and you'll find only the TComponentList variants have
  had their Count property adjusted accordingly - the other two still thing the button
  just destroyed already exists, potentially causing an access violation.

  Note: the substantive code is identical to the FireMonkey version.
}
interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.Contnrs, System.Generics.Collections, CCR.Generics.CompList;

type
  TfrmVCL = class(TForm)
    btnContnrs: TButton;
    btnGenericList: TButton;
    btnGenericObjectList: TButton;
    btnTest: TButton;
    btnGenericComponentList: TButton;
    procedure btnContnrsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnGenericListClick(Sender: TObject);
    procedure btnGenericObjectListClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure btnGenericComponentListClick(Sender: TObject);
  strict private
    FComponentList: TComponentList;
    FList: TList<TControl>;
    FObjectList: TObjectList<TControl>;
    FGenericComponentList: TComponentList<TControl>;
  end;

var
  frmVCL: TfrmVCL;

implementation

{$R *.dfm}

procedure TfrmVCL.FormCreate(Sender: TObject);
var
  Comp: TComponent;
begin
  Application.Title := Caption;
  { initialise each list, adding to it references to the buttons on the form }
  FComponentList := TComponentList.Create;
  FList := TList<TControl>.Create;
  FObjectList := TObjectList<TControl>.Create(False);
  FGenericComponentList := TComponentList<TControl>.Create;
  for Comp in Self do
    if Comp is TButton then
    begin
      FComponentList.Add(Comp);
      FList.Add(TControl(Comp));
      FObjectList.Add(TControl(Comp));
      FGenericComponentList.Add(TControl(Comp));
    end;
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TfrmVCL.FormDestroy(Sender: TObject);
begin
  FComponentList.Free;
  FList.Free;
  FObjectList.Free;
  FGenericComponentList.Free;
end;

procedure TfrmVCL.btnContnrsClick(Sender: TObject);
begin
  ShowMessageFmt('The stock TComponentList reckons the form has %d buttons',
   [FComponentList.Count]);
end;

procedure TfrmVCL.btnGenericListClick(Sender: TObject);
begin
  ShowMessageFmt('TList reckons the form has %d buttons',
    [FList.Count]);
end;

procedure TfrmVCL.btnGenericObjectListClick(Sender: TObject);
begin
  ShowMessageFmt('TObjectList reckons the form has %d buttons',
    [FObjectList.Count]);
end;

procedure TfrmVCL.btnGenericComponentListClick(Sender: TObject);
begin
  ShowMessageFmt('Our generic TComponentList reckons the form has %d buttons',
    [FGenericComponentList.Count]);
end;

procedure TfrmVCL.btnTestClick(Sender: TObject);
begin
  FreeAndNil(btnTest);
end;

end.

unit MainForm;
{
  FireMonkey version of the VCL TComponentList demo - see the VCL version for
  more info.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts,
  System.Contnrs, System.Generics.Collections, CCR.Generics.CompList;


type
  TfrmFMX = class(TForm)
    btnContnrs: TButton;
    btnGenericList: TButton;
    btnGenericObjectList: TButton;
    btnGenericComponentList: TButton;
    btnTest: TButton;
    lyoBase: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnContnrsClick(Sender: TObject);
    procedure btnGenericListClick(Sender: TObject);
    procedure btnGenericObjectListClick(Sender: TObject);
    procedure btnGenericComponentListClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
  strict private
    FComponentList: TComponentList;
    FList: TList<TControl>;
    FObjectList: TObjectList<TControl>;
    FGenericComponentList: TComponentList<TControl>;
  end;

var
  frmFMX: TfrmFMX;

implementation

{$R *.fmx}

uses CCR.FMXUtils;

procedure TfrmFMX.FormCreate(Sender: TObject);
var
  Comp: TComponent;
begin
  Application.Title := Caption;
  ScaleFormForScreen(lyoBase);
  { initialise each list, adding to it a reference to btnTest }
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

procedure TfrmFMX.FormDestroy(Sender: TObject);
begin
  FComponentList.Free;
  FList.Free;
  FObjectList.Free;
  FGenericComponentList.Free;
end;

procedure TfrmFMX.btnContnrsClick(Sender: TObject);
begin
  ShowMessageFmt('The stock TComponentList reckons the form has %d buttons',
   [FComponentList.Count]);
end;

procedure TfrmFMX.btnGenericListClick(Sender: TObject);
begin
  ShowMessageFmt('TList reckons the form has %d buttons',
    [FList.Count]);
end;

procedure TfrmFMX.btnGenericObjectListClick(Sender: TObject);
begin
  ShowMessageFmt('TObjectList reckons the form has %d buttons',
    [FObjectList.Count]);
end;

procedure TfrmFMX.btnGenericComponentListClick(Sender: TObject);
begin
  ShowMessageFmt('Our generic TComponentList reckons the form has %d buttons',
    [FGenericComponentList.Count]);
end;

procedure TfrmFMX.btnTestClick(Sender: TObject);
begin
  FreeAndNil(btnTest);
end;

end.

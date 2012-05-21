unit ColorSwitchForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Objects, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo;

type
  TFormColorSwitcher = class(TThread)
  strict private
    FForm: TForm;
  protected
    procedure Execute; override;
  public
    constructor Create(AForm: TForm);
  end;

  TfrmColorSwitcher = class(TForm)
    Memo1: TMemo;
    chkPauseSwitchingColor: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Memo1ApplyStyleLookup(Sender: TObject);
    procedure chkPauseSwitchingColorChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict private
    FBkgThread: TThread;
  end;

var
  frmColorSwitcher: TfrmColorSwitcher;

implementation

{$R *.fmx}

{ TFormColorSwitcher }

constructor TFormColorSwitcher.Create(AForm: TForm);
begin
  inherited Create;
  FForm := AForm;
end;

procedure TFormColorSwitcher.Execute;

  function RandomElem: Byte;
  begin
    Result := Random(100) + 155;
  end;
var
  NewColor: TAlphaColor;
begin
  while not Terminated do
  begin
    Sleep(500);
    NewColor := MakeColor(RandomElem, RandomElem, RandomElem);
    Synchronize(
      procedure
      begin
        FForm.Fill.Color := NewColor;
      end);
  end;
end;

{ TfrmColorSwitcher }

procedure TfrmColorSwitcher.chkPauseSwitchingColorChange(Sender: TObject);
begin
  if chkPauseSwitchingColor.IsChecked then
  begin
    FBkgThread.Terminate;
    FBkgThread.WaitFor;
    FreeAndNil(FBkgThread);
    Exit;
  end;
  FBkgThread := TFormColorSwitcher.Create(Self);
end;

procedure TfrmColorSwitcher.FormCreate(Sender: TObject);
begin
  FBkgThread := TFormColorSwitcher.Create(Self);
end;

procedure TfrmColorSwitcher.FormDestroy(Sender: TObject);
begin
  if FBkgThread <> nil then
  begin
    FBkgThread.Terminate;
    FBkgThread.WaitFor;
    FBkgThread.Free;
  end;
end;

procedure TfrmColorSwitcher.Memo1ApplyStyleLookup(Sender: TObject);
begin
  ((Sender as TFmxObject).FindStyleResource('background') as TRectangle).Opacity := 0.5;
end;

end.

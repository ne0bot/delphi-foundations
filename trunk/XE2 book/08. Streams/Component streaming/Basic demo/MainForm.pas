unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Objects;

type
  TfrmBasicStreamingDemo = class(TForm)
    scbHost: TFramedScrollBox;
    btnAdd: TButton;
    btnClear: TButton;
    btnSave: TButton;
    btnReload: TButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnReloadClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBasicStreamingDemo: TfrmBasicStreamingDemo;

implementation

{$R *.fmx}

procedure TfrmBasicStreamingDemo.btnAddClick(Sender: TObject);
var
  NewObject: TSelection;
begin
  NewObject := TSelection.Create(scbHost);
  NewObject.Position.X := 20 * scbHost.ComponentCount;
  NewObject.Position.Y := 20 * scbHost.ComponentCount;
  NewObject.Parent := scbHost;
end;

procedure TfrmBasicStreamingDemo.btnClearClick(Sender: TObject);
var
  I: Integer;
begin
  for I := scbHost.ComponentCount - 1 downto 0 do
    if scbHost.Components[I] is TSelection then
      scbHost.Components[I].Free;
end;

function GetLayoutFileName: string;
begin
  Result := ChangeFileExt(ParamStr(0), '.layout');
end;

procedure TfrmBasicStreamingDemo.btnSaveClick(Sender: TObject);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(GetLayoutFileName, fmCreate);
  try
    Stream.WriteComponent(scbHost);
  finally
    Stream.Free;
  end;
end;

procedure TfrmBasicStreamingDemo.btnReloadClick(Sender: TObject);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(GetLayoutFileName, fmOpenRead);
  try
    Stream.ReadComponent(scbHost);
  finally
    Stream.Free;
  end;
end;

end.

unit SysInfoFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit,
  PluginIntf, FMX.Layouts, FMX.Memo;

type
  TfrmSystemInfo = class(TForm, IPluginFrame)
    lyoBase: TLayout;
    Label1: TLabel;
    edtOS: TEdit;
    Label3: TLabel;
    edtCPUCoreCount: TEdit;
    memPathEnvVar: TMemo;
    Label2: TLabel;
    Label4: TLabel;
    edtHostAppTitle: TEdit;
    edtHostAppPath: TEdit;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure lyoBaseResize(Sender: TObject);
  protected
    function GetBaseControl: TControl;
  end;

var
  frmSystemInfo: TfrmSystemInfo;

implementation

{$R *.fmx}

procedure TfrmSystemInfo.FormCreate(Sender: TObject);
begin
  edtOS.Text := TOSVersion.ToString;
  edtCPUCoreCount.Text := IntToStr(TThread.ProcessorCount);
  memPathEnvVar.Lines.Delimiter := PathSep;
  memPathEnvVar.Lines.DelimitedText := GetEnvironmentVariable('PATH');
  edtHostAppTitle.Text := Application.Title;
  edtHostAppPath.Text := GetModuleName(MainInstance);
end;

function TfrmSystemInfo.GetBaseControl: TControl;
begin
  Result := lyoBase;
end;

procedure TfrmSystemInfo.lyoBaseResize(Sender: TObject);
var
  I: Integer;
  Ctrl: TControl;
begin
  for I := 0 to lyoBase.ChildrenCount - 1 do
    if (lyoBase.Children[I] is TControl) then
    begin
      Ctrl := TControl(lyoBase.Children[I]);
      if Ctrl.Tag = 1 then Ctrl.Width := lyoBase.Width - Ctrl.Position.X;
    end;
end;

end.

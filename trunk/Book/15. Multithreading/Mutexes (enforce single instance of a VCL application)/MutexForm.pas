unit MutexForm;
{
  Example of using a mutex to enforce a single instance VCL application. The substantive
  code is in SingleInstVCL.pas.
}
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmMutexDemo = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    memCommandLines: TMemo;
    procedure FormCreate(Sender: TObject);
  end;

var
  frmMutexDemo: TfrmMutexDemo;

implementation

{$R *.dfm}

uses SingleInstVCL;

procedure TfrmMutexDemo.FormCreate(Sender: TObject);
begin
  OnAttemptedReopen :=
    procedure (const ACommandLine: string)
    begin
      memCommandLines.Lines.Add(TimeToStr(Time) + ':');
      memCommandLines.Lines.Add(ACommandLine);
      memCommandLines.Lines.Add('');
    end;
end;

end.

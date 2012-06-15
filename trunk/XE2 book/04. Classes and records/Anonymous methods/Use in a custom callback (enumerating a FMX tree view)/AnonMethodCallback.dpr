program AnonMethodCallback;

uses
  FMX.Forms,
  EnumForm in 'EnumForm.pas' {frmEnumTreeView};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmEnumTreeView, frmEnumTreeView);
  Application.Run;
end.

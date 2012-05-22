unit MemoFrame;
{
  NB: the package output directory in Project Options has been set to

    ..\Binaries\$(Platform)\$(Config)

  and the search path to
    ..\Shared package\$(Platform)\$(Config)

  Lastly, Shared has been added to the DPK's requires clause.
}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo,
  PluginIntf;

type
  TfrmMemo = class(TForm, IPluginFrame)
    Memo: TMemo;
  protected
    function GetBaseControl: TControl;
  end;

implementation

{$R *.fmx}

function TfrmMemo.GetBaseControl: TControl;
begin
  Result := Memo;
end;

end.

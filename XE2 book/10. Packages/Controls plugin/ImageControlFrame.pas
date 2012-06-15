unit ImageControlFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, PluginIntf;

type
  TfrmImageControl = class(TForm, IPluginFrame)
    ImageControl: TImageControl;
  protected
    function GetBaseControl: TControl;
  end;

implementation

{$R *.fmx}

function TfrmImageControl.GetBaseControl: TControl;
begin
  Result := ImageControl;
end;

end.

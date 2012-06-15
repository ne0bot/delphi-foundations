unit InjectedStatusBar;

interface

implementation

uses
  FMX.Types, FMX.Controls, FMX.Forms, InjectedControlsHelper;

var
  StatusBar: TStatusBar;
  LabelControl: TLabel;

initialization
  StatusBar := TInjectedControl<TStatusBar>.Create;
  StatusBar.Align := TAlignLayout.alBottom;
  {$IFDEF MACOS}
  StatusBar.ShowSizeGrip := False;
  {$ENDIF}
  LabelControl := TLabel.Create(StatusBar);
  LabelControl.Align := TAlignLayout.alClient;
  LabelControl.VertTextAlign := TTextAlign.taCenter;
  LabelControl.Text := ' This status bar has been injected into the host ' +
    'application''s main form by the plugin';
  StatusBar.AddObject(LabelControl);
  Application.MainForm.AddObject(StatusBar);
end.

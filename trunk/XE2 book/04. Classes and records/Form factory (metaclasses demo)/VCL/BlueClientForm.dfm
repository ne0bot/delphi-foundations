object frmBlueClient: TfrmBlueClient
  Left = 825
  Top = 34
  Caption = 'Blue Form'
  ClientHeight = 110
  ClientWidth = 292
  Color = clBlue
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    292
    110)
  PixelsPerInch = 120
  TextHeight = 20
  object btnClose: TButton
    Left = 203
    Top = 37
    Width = 81
    Height = 33
    Anchors = [akTop, akRight]
    Caption = 'Close'
    TabOrder = 0
    OnClick = btnCloseClick
  end
end

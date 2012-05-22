object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 86
  ClientWidth = 171
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 67
    Width = 171
    Height = 19
    Panels = <
      item
        Text = 'Second'
        Width = 50
      end
      item
        Text = 'Third'
        Width = 50
      end
      item
        Text = 'First'
        Width = 50
      end>
    ExplicitLeft = 248
    ExplicitTop = 104
    ExplicitWidth = 0
  end
  object Button1: TButton
    Left = 32
    Top = 22
    Width = 113
    Height = 25
    Caption = 'Sort Status Panels'
    TabOrder = 1
    OnClick = Button1Click
  end
end

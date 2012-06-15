object ObserverFrame: TObserverFrame
  Left = 0
  Top = 0
  Width = 255
  Height = 240
  TabOrder = 0
  object Label1: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 243
    Height = 13
    Align = alTop
    Caption = 'This is a frame object that observes button 2 clicks'
  end
  object memOutput: TMemo
    Left = 0
    Top = 19
    Width = 255
    Height = 221
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 72
    ExplicitTop = 80
    ExplicitWidth = 185
    ExplicitHeight = 89
  end
end

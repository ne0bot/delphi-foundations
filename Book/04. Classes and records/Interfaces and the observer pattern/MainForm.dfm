object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Interfaced Observer Demo'
  ClientHeight = 382
  ClientWidth = 299
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 73
    Width = 129
    Height = 49
    Caption = 'Send notification of button 1 being clicked'
    TabOrder = 2
    WordWrap = True
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 160
    Top = 73
    Width = 129
    Height = 49
    Caption = 'Send notification of button 2 being clicked'
    TabOrder = 3
    WordWrap = True
    OnClick = Button2Click
  end
  object btnCreateButton1Observer: TButton
    Left = 8
    Top = 8
    Width = 281
    Height = 25
    Caption = 'Create form that listens only for button 1 being clicked'
    TabOrder = 0
    OnClick = btnCreateButton1ObserverClick
  end
  object btnCreateButton1And2Observer: TButton
    Left = 8
    Top = 39
    Width = 281
    Height = 25
    Caption = 'Create form that listens for button 1 and 2 being clicked'
    TabOrder = 1
    OnClick = btnCreateButton1And2ObserverClick
  end
  inline ObserverFrame1: TObserverFrame
    Left = 8
    Top = 133
    Width = 281
    Height = 240
    TabOrder = 4
    ExplicitLeft = 8
    ExplicitTop = 133
    ExplicitWidth = 281
    inherited Label1: TLabel
      Width = 275
    end
    inherited memOutput: TMemo
      Width = 281
      ExplicitLeft = 0
      ExplicitTop = 19
      ExplicitWidth = 281
      ExplicitHeight = 221
    end
  end
end

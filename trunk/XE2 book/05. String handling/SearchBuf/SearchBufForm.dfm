object frmSearchBufDemo: TfrmSearchBufDemo
  Left = 272
  Top = 106
  Caption = 'SearchBuf Demo'
  ClientHeight = 436
  ClientWidth = 611
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShortCut = FormShortCut
  PixelsPerInch = 96
  TextHeight = 15
  object Memo: TRichEdit
    AlignWithMargins = True
    Left = 2
    Top = 2
    Width = 607
    Height = 402
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    HideSelection = False
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitWidth = 590
  end
  object Panel1: TPanel
    Left = 0
    Top = 406
    Width = 611
    Height = 30
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 594
    object Label1: TLabel
      Left = 5
      Top = 5
      Width = 94
      Height = 15
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = '&Text to search for:'
      FocusControl = edtSearchString
    end
    object edtSearchString: TEdit
      Left = 103
      Top = 2
      Width = 113
      Height = 23
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      TabOrder = 0
      Text = 'and'
      OnKeyDown = edtSearchStringKeyDown
      OnKeyPress = edtSearchStringKeyPress
    end
    object btnFindNext: TButton
      Left = 221
      Top = 2
      Width = 86
      Height = 23
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Find &Next'
      TabOrder = 1
      OnClick = btnFindNextClick
    end
    object btnFindPrevious: TButton
      Left = 311
      Top = 2
      Width = 86
      Height = 23
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Find &Previous'
      TabOrder = 2
      OnClick = btnFindPreviousClick
    end
    object chkMatchCase: TCheckBox
      Left = 402
      Top = 7
      Width = 79
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Match &case'
      TabOrder = 3
    end
    object chkMatchWholeWord: TCheckBox
      Left = 485
      Top = 7
      Width = 122
      Height = 13
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Match &whole word'
      TabOrder = 4
    end
  end
end

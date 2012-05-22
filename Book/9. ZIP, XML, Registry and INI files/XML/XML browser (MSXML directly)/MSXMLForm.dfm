object frmMSXML: TfrmMSXML
  Left = 0
  Top = 0
  Caption = 'XML Parsing with MSXML Directly'
  ClientHeight = 452
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 265
    Top = 41
    Height = 411
    ResizeStyle = rsUpdate
    ExplicitLeft = 272
    ExplicitTop = 192
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 527
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btnLoad: TButton
      Left = 8
      Top = 8
      Width = 89
      Height = 25
      Caption = 'Load XML...'
      TabOrder = 0
      OnClick = btnLoadClick
    end
  end
  object trvOutline: TTreeView
    Left = 0
    Top = 41
    Width = 265
    Height = 411
    Align = alLeft
    Indent = 19
    ReadOnly = True
    TabOrder = 1
    OnChange = trvOutlineChange
    OnCreateNodeClass = trvOutlineCreateNodeClass
  end
  object memContent: TRichEdit
    Left = 268
    Top = 41
    Width = 259
    Height = 411
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    PlainText = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'xml'
    Filter = 'XML files|*.xml'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 72
    Top = 128
  end
end

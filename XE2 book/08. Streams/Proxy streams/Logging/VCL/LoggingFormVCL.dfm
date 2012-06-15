object frmLogProxyVCL: TfrmLogProxyVCL
  Left = 0
  Top = 0
  Caption = 'Logging Proxy Stream Demo (VCL)'
  ClientHeight = 347
  ClientWidth = 726
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Padding.Left = 6
  Padding.Top = 6
  Padding.Right = 6
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 321
    Top = 6
    Height = 300
    ResizeStyle = rsUpdate
    ExplicitLeft = 282
    ExplicitHeight = 252
  end
  object Panel1: TPanel
    Left = 6
    Top = 306
    Width = 714
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 0
      Top = 12
      Width = 79
      Height = 15
      Caption = '&DOM backend:'
      FocusControl = cboDOMBackend
    end
    object btnLoadXML: TButton
      Left = 255
      Top = 8
      Width = 106
      Height = 26
      Caption = 'Load XML File...'
      TabOrder = 0
      OnClick = btnLoadXMLClick
    end
    object cboDOMBackend: TComboBox
      Left = 90
      Top = 9
      Width = 159
      Height = 23
      Style = csDropDownList
      TabOrder = 1
      OnChange = cboDOMBackendChange
    end
  end
  object grpXML: TGroupBox
    Left = 324
    Top = 6
    Width = 396
    Height = 300
    Align = alClient
    Caption = ' XML '
    Padding.Left = 7
    Padding.Top = 1
    Padding.Right = 7
    Padding.Bottom = 7
    TabOrder = 2
    object memXML: TRichEdit
      Left = 9
      Top = 18
      Width = 378
      Height = 273
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
  object grpLog: TGroupBox
    Left = 6
    Top = 6
    Width = 315
    Height = 300
    Align = alLeft
    Caption = ' Log '
    Padding.Left = 7
    Padding.Top = 1
    Padding.Right = 7
    Padding.Bottom = 7
    TabOrder = 1
    object lsbLog: TListBox
      Left = 9
      Top = 18
      Width = 297
      Height = 273
      Style = lbVirtual
      Align = alClient
      TabOrder = 0
      OnData = lsbLogData
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'xml'
    Filter = 'XML files|*.xml;*.xmp;*.xhtml;*.css'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 80
    Top = 192
  end
end

object frmXML: TfrmXML
  Left = 0
  Top = 0
  Caption = 'Zlib Compression Test'
  ClientHeight = 427
  ClientWidth = 658
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 390
    Width = 658
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnLoadSource: TButton
      Left = 8
      Top = 6
      Width = 121
      Height = 25
      Caption = '&Load source XML...'
      TabOrder = 0
      OnClick = btnLoadSourceClick
    end
    object btnCalcCompressedSize: TButton
      Left = 135
      Top = 6
      Width = 146
      Height = 25
      Caption = '&Calculate compressed size'
      TabOrder = 1
      OnClick = btnCalcCompressedSizeClick
    end
    object btnResaveCompressed: TButton
      Left = 287
      Top = 6
      Width = 138
      Height = 25
      Caption = '&Resave compressed...'
      TabOrder = 2
      OnClick = btnResaveCompressedClick
    end
  end
  object redDisplay: TRichEdit
    Left = 0
    Top = 0
    Width = 658
    Height = 390
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    PlainText = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'xml'
    Filter = 'XML files (*.xml, *.xmlc)|*.xml;*xmlc'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 320
    Top = 216
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'xmlc'
    Filter = 'Compressed XML files (*.xmlc)|*.xmlc'
    Left = 392
    Top = 216
  end
end

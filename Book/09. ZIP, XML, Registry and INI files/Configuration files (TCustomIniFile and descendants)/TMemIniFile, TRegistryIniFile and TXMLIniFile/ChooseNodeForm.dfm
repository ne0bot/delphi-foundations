object frmChooseNode: TfrmChooseNode
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Select Root Node'
  ClientHeight = 107
  ClientWidth = 240
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 15
  object lblPrompt: TLabel
    Left = 8
    Top = 8
    Width = 228
    Height = 33
    AutoSize = False
    Caption = 
      'Choose the node to use as the root - child nodes will be interpr' +
      'eted as INI sections):'
    WordWrap = True
  end
  object cboNodes: TComboBox
    Left = 8
    Top = 44
    Width = 224
    Height = 23
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 0
    Text = '<Document element>'
    Items.Strings = (
      '<Document element>')
  end
  object btnOK: TButton
    Left = 76
    Top = 74
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 157
    Top = 74
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end

object frmNameDlg: TfrmNameDlg
  Left = 931
  Top = 436
  BorderStyle = bsDialog
  Caption = 'Form Test'
  ClientHeight = 102
  ClientWidth = 211
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 120
  TextHeight = 20
  object edtName: TLabeledEdit
    Left = 10
    Top = 28
    Width = 190
    Height = 28
    EditLabel.Width = 111
    EditLabel.Height = 20
    EditLabel.Caption = 'Enter your name:'
    TabOrder = 0
    OnChange = edtNameChange
  end
  object btnOK: TButton
    Left = 32
    Top = 64
    Width = 81
    Height = 28
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 119
    Top = 64
    Width = 81
    Height = 28
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end

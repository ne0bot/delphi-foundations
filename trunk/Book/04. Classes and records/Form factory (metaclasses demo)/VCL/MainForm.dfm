object frmMain: TfrmMain
  Left = 503
  Top = 221
  Caption = 'VCL Form Factory Example'
  ClientHeight = 234
  ClientWidth = 522
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 20
  object lsvForms: TListView
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 406
    Height = 228
    Align = alClient
    Columns = <
      item
        Caption = 'Name'
        Width = 100
      end
      item
        Caption = 'Description'
        Width = 300
      end>
    ColumnClick = False
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = btnCreateFormClick
  end
  object panRight: TPanel
    Left = 412
    Top = 0
    Width = 110
    Height = 234
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object btnCreateForm: TButton
      Left = 3
      Top = 3
      Width = 102
      Height = 30
      Caption = 'Create Form'
      Default = True
      TabOrder = 0
      OnClick = btnCreateFormClick
    end
  end
end

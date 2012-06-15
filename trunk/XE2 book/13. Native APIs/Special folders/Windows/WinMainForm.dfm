object frmWinSpecialFolders: TfrmWinSpecialFolders
  Left = 0
  Top = 0
  Caption = 'Special Folders (Windows)'
  ClientHeight = 506
  ClientWidth = 738
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lsvFolders: TListView
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 732
    Height = 500
    Align = alClient
    Columns = <
      item
        Caption = 'Directory Type'
        Width = 200
      end
      item
        Caption = 'Path'
        Width = 500
      end>
    ColumnClick = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    ExplicitWidth = 634
    ExplicitHeight = 332
  end
end

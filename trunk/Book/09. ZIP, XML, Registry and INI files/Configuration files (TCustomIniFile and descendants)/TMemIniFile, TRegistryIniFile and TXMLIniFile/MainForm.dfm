object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'TCustomIniFile Polymorphism Demo'
  ClientHeight = 503
  ClientWidth = 895
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Splitter: TSplitter
    Left = 236
    Top = 44
    Height = 440
    ResizeStyle = rsUpdate
    ExplicitLeft = 392
    ExplicitTop = 184
    ExplicitHeight = 100
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 895
    Height = 44
    BorderWidth = 1
    ButtonHeight = 38
    ButtonWidth = 137
    Color = clGray
    DisabledImages = dtmActions.imlDisabled
    DrawingStyle = dsGradient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    GradientEndColor = clBlack
    GradientStartColor = clGray
    HotTrackColor = clHighlight
    Images = dtmActions.imlNormal
    List = True
    ParentColor = False
    ParentFont = False
    ShowCaptions = True
    TabOrder = 0
    Wrapable = False
    object btnLoad: TToolButton
      Left = 0
      Top = 0
      Action = dtmActions.actLoadFromIniFile
      AutoSize = True
      DropdownMenu = mnuLoad
      Style = tbsDropDown
    end
    object btnSave: TToolButton
      Left = 156
      Top = 0
      Action = dtmActions.actSaveAsIniFile
      AutoSize = True
      DropdownMenu = mnuSave
      Style = tbsDropDown
    end
    object sep1: TToolButton
      Left = 293
      Top = 0
      Width = 8
      Caption = 'sep1'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object btnAddSection: TToolButton
      Left = 301
      Top = 0
      Action = dtmActions.actAddSection
      AutoSize = True
    end
    object btnRenameSection: TToolButton
      Left = 404
      Top = 0
      Action = dtmActions.actRenameSection
      AutoSize = True
      Style = tbsTextButton
    end
    object btnDeleteSection: TToolButton
      Left = 500
      Top = 0
      Action = dtmActions.actDeleteSection
      AutoSize = True
    end
    object sep2: TToolButton
      Left = 614
      Top = 0
      Width = 8
      Caption = 'sep2'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object btnAddKey: TToolButton
      Left = 622
      Top = 0
      Action = dtmActions.actAddKey
      AutoSize = True
    end
    object btnEditKey: TToolButton
      Left = 705
      Top = 0
      Action = dtmActions.actEditKey
      AutoSize = True
    end
    object btnDeleteKey: TToolButton
      Left = 786
      Top = 0
      Action = dtmActions.actDeleteKey
      AutoSize = True
    end
  end
  object panSections: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 47
    Width = 233
    Height = 434
    Margins.Right = 0
    Align = alLeft
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = ' Sections'
    DoubleBuffered = True
    Padding.Top = 20
    ParentDoubleBuffered = False
    TabOrder = 1
    VerticalAlignment = taAlignTop
    object lsvSections: TListView
      Left = 0
      Top = 20
      Width = 233
      Height = 414
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          Width = 200
        end>
      ColumnClick = False
      HideSelection = False
      OwnerData = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnData = lsvSectionsData
      OnDataFind = lsvSectionsDataFind
      OnDblClick = lsvSectionsDblClick
      OnKeyDown = lsvSectionsKeyDown
      OnSelectItem = lsvSectionsSelectItem
    end
  end
  object panKeys: TPanel
    AlignWithMargins = True
    Left = 239
    Top = 47
    Width = 653
    Height = 434
    Margins.Left = 0
    Align = alClient
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = ' Keys'
    DoubleBuffered = True
    Padding.Top = 20
    ParentDoubleBuffered = False
    TabOrder = 2
    VerticalAlignment = taAlignTop
    object lsvKeys: TListView
      Left = 0
      Top = 20
      Width = 653
      Height = 414
      Align = alClient
      Columns = <
        item
          Caption = 'Key'
          Width = 200
        end
        item
          Caption = 'Value'
          Width = 400
        end>
      ColumnClick = False
      HideSelection = False
      OwnerData = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnData = lsvKeysData
      OnDataFind = lsvKeysDataFind
      OnDblClick = lsvKeysDblClick
      OnKeyDown = lsvKeysKeyDown
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 484
    Width = 895
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object mnuLoad: TPopupMenu
    Left = 384
    Top = 216
    object itmLoadFromIniFile: TMenuItem
      Action = dtmActions.actLoadFromIniFile
    end
    object itmLoadFromRegistry: TMenuItem
      Action = dtmActions.actLoadFromRegistry
    end
    object itmLoadFromXMLFile: TMenuItem
      Action = dtmActions.actLoadFromXML
    end
  end
  object mnuSave: TPopupMenu
    AutoHotkeys = maManual
    Left = 456
    Top = 216
    object itmSaveAsIniFile: TMenuItem
      Action = dtmActions.actSaveAsIniFile
    end
    object itmSaveAsXML: TMenuItem
      Action = dtmActions.actSaveAsXML
    end
  end
end

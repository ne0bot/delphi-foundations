object frmFileSearchVCL: TfrmFileSearchVCL
  Left = 272
  Top = 106
  Caption = 'Threaded File Search (VCL)'
  ClientHeight = 328
  ClientWidth = 565
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object lsbFoundFiles: TListBox
    AlignWithMargins = True
    Left = 5
    Top = 36
    Width = 555
    Height = 270
    Margins.Left = 5
    Margins.Top = 2
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    ItemHeight = 15
    TabOrder = 1
    OnDblClick = lsbFoundFilesDblClick
    OnKeyPress = lsbFoundFilesKeyPress
  end
  object panTop: TPanel
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 555
    Height = 24
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      555
      24)
    object edtSearchFor: TEdit
      Left = 0
      Top = 2
      Width = 424
      Height = 23
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Anchors = [akLeft, akTop, akRight]
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 0
      Text = 'C:\*.pas'
      OnKeyPress = edtSearchForKeyPress
    end
    object btnSearch: TButton
      Left = 429
      Top = 0
      Width = 61
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Anchors = [akTop, akRight]
      Caption = 'Search'
      TabOrder = 1
      OnClick = btnSearchClick
    end
    object btnStop: TButton
      Left = 495
      Top = 0
      Width = 61
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Stop'
      Enabled = False
      TabOrder = 2
      OnClick = btnStopClick
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 311
    Width = 565
    Height = 17
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Panels = <>
    ParentFont = True
    SimplePanel = True
    SimpleText = 'Press Search to begin finding files'
    UseSystemFont = False
  end
  object tmrCheckForBatch: TTimer
    Enabled = False
    Interval = 250
    OnTimer = tmrCheckForBatchTimer
    Left = 280
    Top = 160
  end
end

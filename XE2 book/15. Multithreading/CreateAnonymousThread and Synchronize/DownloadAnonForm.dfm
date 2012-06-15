object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'CreateAnonymousThread and Synchronize Example'
  ClientHeight = 343
  ClientWidth = 544
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 6
  Padding.Top = 6
  Padding.Right = 6
  Padding.Bottom = 6
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    AlignWithMargins = True
    Left = 6
    Top = 311
    Width = 532
    Height = 26
    Margins.Left = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alBottom
    Caption = 
      'This is a CreateAnonymousThread and Synchronize variant of the O' +
      'nTerminate demo; where the first button freezes the UI, the seco' +
      'nd doesn'#39't'
    WordWrap = True
    ExplicitWidth = 525
  end
  object PageControl1: TPageControl
    Left = 6
    Top = 36
    Width = 532
    Height = 272
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'DelphiFeeds.com'
      object RichEdit1: TRichEdit
        Left = 0
        Top = 0
        Width = 524
        Height = 244
        Align = alClient
        BorderStyle = bsNone
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        PlainText = True
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Embarcadero.com'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 507
      ExplicitHeight = 0
      object RichEdit2: TRichEdit
        Left = 0
        Top = 0
        Width = 524
        Height = 244
        Align = alClient
        BorderStyle = bsNone
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        PlainText = True
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Google.co.uk'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 507
      ExplicitHeight = 0
      object RichEdit3: TRichEdit
        Left = 0
        Top = 0
        Width = 524
        Height = 244
        Align = alClient
        BorderStyle = bsNone
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        PlainText = True
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Guardian.co.uk'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 507
      ExplicitHeight = 0
      object RichEdit4: TRichEdit
        Left = 0
        Top = 0
        Width = 524
        Height = 244
        Align = alClient
        BorderStyle = bsNone
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        PlainText = True
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'NYTimes.com'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 507
      ExplicitHeight = 0
      object RichEdit5: TRichEdit
        Left = 0
        Top = 0
        Width = 524
        Height = 244
        Align = alClient
        BorderStyle = bsNone
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        PlainText = True
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Telegraph.co.uk'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 507
      ExplicitHeight = 0
      object RichEdit6: TRichEdit
        Left = 0
        Top = 0
        Width = 524
        Height = 244
        Align = alClient
        BorderStyle = bsNone
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        PlainText = True
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object Panel1: TPanel
    Left = 6
    Top = 6
    Width = 532
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object btnLoadUnthreaded: TButton
      Left = 0
      Top = 0
      Width = 233
      Height = 25
      Caption = 'Load HTML of homepages without threading'
      TabOrder = 0
      OnClick = btnLoadUnthreadedClick
    end
    object btnLoadThreaded: TButton
      Left = 247
      Top = 0
      Width = 233
      Height = 25
      Caption = 'Load HTML of homepages with threading'
      TabOrder = 1
      OnClick = btnLoadThreadedClick
    end
  end
end

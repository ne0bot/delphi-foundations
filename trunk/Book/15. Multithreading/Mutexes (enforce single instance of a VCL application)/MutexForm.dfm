object frmMutexDemo: TfrmMutexDemo
  Left = 0
  Top = 0
  Caption = 'Mutex Demo'
  ClientHeight = 201
  ClientWidth = 555
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
  object Label1: TLabel
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 539
    Height = 19
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alTop
    Alignment = taCenter
    Caption = 'Only one of me should ever be open!'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 263
  end
  object Label2: TLabel
    AlignWithMargins = True
    Left = 6
    Top = 38
    Width = 543
    Height = 13
    Margins.Left = 6
    Margins.Right = 6
    Align = alTop
    Caption = 'Latest command lines:'
    ExplicitWidth = 106
  end
  object memCommandLines: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 57
    Width = 549
    Height = 141
    Align = alClient
    TabOrder = 0
  end
end

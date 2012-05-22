object frmVCL: TfrmVCL
  Left = 272
  Top = 106
  Caption = 'TComponentList Test (VCL)'
  ClientHeight = 91
  ClientWidth = 843
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 20
  object btnContnrs: TButton
    Left = 8
    Top = 8
    Width = 201
    Height = 34
    Caption = 'TComponentList'
    TabOrder = 0
    OnClick = btnContnrsClick
  end
  object btnGenericList: TButton
    Left = 215
    Top = 8
    Width = 201
    Height = 34
    Caption = 'TList<TComponent>'
    TabOrder = 1
    OnClick = btnGenericListClick
  end
  object btnGenericObjectList: TButton
    Left = 422
    Top = 8
    Width = 201
    Height = 34
    Caption = 'TObjectList<TComponent>'
    TabOrder = 2
    OnClick = btnGenericObjectListClick
  end
  object btnTest: TButton
    Left = 304
    Top = 48
    Width = 225
    Height = 34
    Caption = 'I say goodbye, hostile world'
    TabOrder = 4
    OnClick = btnTestClick
  end
  object btnGenericComponentList: TButton
    Left = 629
    Top = 8
    Width = 201
    Height = 34
    Caption = 'Our generic TComponentList'
    TabOrder = 3
    OnClick = btnGenericComponentListClick
  end
end

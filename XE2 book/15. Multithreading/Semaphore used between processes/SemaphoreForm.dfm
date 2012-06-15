object frmSemaphore: TfrmSemaphore
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Multiprocess Semaphore Demo'
  ClientHeight = 218
  ClientWidth = 288
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
  object lblInfo: TLabel
    Left = 8
    Top = 74
    Width = 267
    Height = 105
    Caption = 
      'On Windows, a TSemaphore can be used as a cross-process signalli' +
      'ng device. In this demo, only three concurrent instances of the ' +
      'application are allowed to have their animation running. If you ' +
      'spawn more than three instances, the fourth will not animate unt' +
      'il one of the earlier three has been closed.'
    WordWrap = True
  end
  object AnimatedCtrl: TAnimate
    Left = 8
    Top = 8
    Width = 272
    Height = 60
    CommonAVI = aviCopyFile
    StopFrame = 22
  end
  object btnSpawnNewInst: TButton
    Left = 64
    Top = 185
    Width = 145
    Height = 25
    Caption = 'Spawn New Instance'
    TabOrder = 1
    OnClick = btnSpawnNewInstClick
  end
end

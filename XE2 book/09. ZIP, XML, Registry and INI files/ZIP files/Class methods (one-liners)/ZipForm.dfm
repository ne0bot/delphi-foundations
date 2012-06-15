object frmZipClassMethods: TfrmZipClassMethods
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'TZipFile class methods demo'
  ClientHeight = 132
  ClientWidth = 211
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 15
  object btnIsValid: TButton
    Left = 8
    Top = 8
    Width = 193
    Height = 25
    Caption = 'Check whether a file is a valid ZIP'
    TabOrder = 0
    OnClick = btnIsValidClick
  end
  object btnExtract: TButton
    Left = 8
    Top = 39
    Width = 193
    Height = 25
    Caption = 'Extract ZIP to folder'
    TabOrder = 1
    OnClick = btnExtractClick
  end
  object btnConvert: TButton
    Left = 8
    Top = 70
    Width = 193
    Height = 25
    Caption = 'Convert folder to ZIP file'
    TabOrder = 2
    OnClick = btnConvertClick
  end
  object btnClose: TButton
    Left = 64
    Top = 101
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 3
    OnClick = btnCloseClick
  end
  object dlgOpenZip: TOpenDialog
    DefaultExt = 'zip'
    Filter = 
      'ZIP files|*.zip|MS Office 2007+ files|*.docx;*.docm;*.pptx;*.ppt' +
      'm;*.xlsx;*.xlsm|Java archives (*.jar)|*.jar|Silverlight applicat' +
      'ion packages (*.xap)|*.xap|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 16
    Top = 136
  end
  object dlgSelectDirVistaPlus: TFileOpenDialog
    FavoriteLinks = <>
    FileName = 
      'C:\Users\CCR\Documents\RAD Studio\Projects\Book examples\Registr' +
      'y\Win32'
    FileTypes = <>
    Options = [fdoPickFolders]
    Left = 88
    Top = 152
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'zip'
    Filter = 'ZIP files|*.zip'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Step 2: choose ZIP file name'
    Left = 152
    Top = 136
  end
end

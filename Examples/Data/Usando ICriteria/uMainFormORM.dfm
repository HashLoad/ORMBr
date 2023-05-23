object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 451
  ClientWidth = 864
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnOpen: TButton
    Left = 18
    Top = 388
    Width = 163
    Height = 25
    Caption = 'ICriteria Execute (All)'
    TabOrder = 0
    OnClick = btnOpenClick
  end
  object Memo1: TMemo
    Left = 18
    Top = 15
    Width = 817
    Height = 367
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Button1: TButton
    Left = 187
    Top = 388
    Width = 163
    Height = 25
    Caption = 'ICriteria Execute (Where)'
    TabOrder = 2
    OnClick = Button1Click
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=..\Database\database.db3'
      'DriverID=SQLite')
    UpdateOptions.AssignedValues = [uvLockWait]
    TxOptions.Isolation = xiSnapshot
    LoginPrompt = False
    Left = 151
    Top = 15
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 218
    Top = 42
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 86
    Top = 42
  end
end

object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 501
  ClientWidth = 811
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
  object Memo1: TMemo
    Left = 4
    Top = 4
    Width = 799
    Height = 458
    TabOrder = 0
  end
  object Button1: TButton
    Left = 4
    Top = 468
    Width = 133
    Height = 25
    Caption = 'Compare Metadata'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 153
    Top = 468
    Width = 124
    Height = 25
    Caption = 'GetTableNames'
    TabOrder = 2
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'FMX'
    Left = 234
    Top = 26
  end
  object FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink
    Left = 340
    Top = 26
  end
  object FDPhysPgDriverLink1: TFDPhysPgDriverLink
    Left = 340
    Top = 80
  end
  object FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink
    Left = 340
    Top = 140
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 338
    Top = 204
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 338
    Top = 256
  end
  object FDPhysIBDriverLink1: TFDPhysIBDriverLink
    Left = 338
    Top = 314
  end
  object FDPhysOracleDriverLink1: TFDPhysOracleDriverLink
    Left = 338
    Top = 376
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=D:\ORMBr\Demo\Data\Database\database.fdb'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'CharacterSet=WIN1252'
      'DriverID=FB')
    Left = 114
    Top = 33
  end
  object FDConnection2: TFDConnection
    Params.Strings = (
      'Database=D:\ORMBr\Demo\Data\Database\database.fdb'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'CharacterSet=WIN1252'
      'DriverID=FB')
    Left = 114
    Top = 102
  end
  object FDMetaInfoQuery1: TFDMetaInfoQuery
    Connection = FDConnection1
    Left = 624
    Top = 318
  end
end

object ServerDataModule: TServerDataModule
  OldCreateOrder = False
  Height = 203
  Width = 400
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 192
    Top = 42
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 134
    Top = 104
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      
        'Database=D:\ORMBr RESTful Components\Demo\Data\Database\database' +
        '.fdb'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'DriverID=FB')
    LoginPrompt = False
    Left = 86
    Top = 40
  end
end

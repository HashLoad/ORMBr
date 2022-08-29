object ServerDataModule: TServerDataModule
  OldCreateOrder = False
  Height = 303
  Width = 362
  object FDConnection1: TFDConnection
    Params.Strings = (
      
        'Database=D:\PROJETOS-Brasil\ORMBr\Examples\Data\Database\databas' +
        'e.fdb'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'DriverID=FB')
    LoginPrompt = False
    Left = 86
    Top = 48
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 172
    Top = 48
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 126
    Top = 134
  end
end

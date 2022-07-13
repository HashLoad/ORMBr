object ServerDataModule: TServerDataModule
  OldCreateOrder = False
  Height = 287
  Width = 391
  object FDConnection1: TFDConnection
    Params.Strings = (
      'DriverID=SQLite'
      'Database=..\..\..\Data\Database\database.db3')
    LoginPrompt = False
    Left = 110
    Top = 70
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 180
    Top = 128
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 232
    Top = 72
  end
end

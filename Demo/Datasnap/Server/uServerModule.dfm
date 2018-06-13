object ORMBr: TORMBr
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 266
  Width = 372
  object FDConnection1: TFDConnection
    Params.Strings = (
      'DriverID=SQLite'
      'Database=..\..\Data\Database\database.db3')
    LoginPrompt = False
    Left = 156
    Top = 100
  end
end

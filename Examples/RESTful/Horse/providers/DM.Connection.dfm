object DMConn: TDMConn
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 295
  Width = 510
  PixelsPerInch = 120
  object FDConnection1: TFDConnection
    Params.Strings = (
      'User_Name=sysdba'
      'Password=masterkey'
      
        'Database=D:\PROJETOS-Brasil\ORMBr\Examples\RESTful\Data\Database' +
        '\database.fdb'
      'DriverID=FB')
    LoginPrompt = False
    Left = 240
    Top = 130
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 310
    Top = 130
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Console'
    Left = 233
    Top = 220
  end
end

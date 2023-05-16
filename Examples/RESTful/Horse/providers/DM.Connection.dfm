object DMConn: TDMConn
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 236
  Width = 408
  object FDConnection1: TFDConnection
    Params.Strings = (
      'User_Name=sysdba'
      'Password=masterkey'
      
        'Database=D:\PROJETOS-Brasil\ORMBr RESTful\Examples\Horse\databas' +
        'e.fdb'
      'DriverID=FB')
    LoginPrompt = False
    Left = 192
    Top = 104
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 248
    Top = 104
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Console'
    Left = 186
    Top = 176
  end
end

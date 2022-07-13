object ServerDataModule: TServerDataModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Encoding = esASCII
  Height = 286
  Width = 267
  object FDConnection1: TFDConnection
    Params.Strings = (
      
        'Database=D:\ORMBr RESTful Components\Demo\Data\Database\database' +
        '.db3'
      'DriverID=SQLite')
    LoginPrompt = False
    Left = 62
    Top = 42
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 162
    Top = 40
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 118
    Top = 100
  end
  object Master: TDWServerEvents
    IgnoreInvalidParams = False
    Events = <
      item
        Routes = [crAll]
        DWParams = <>
        JsonMode = jmPureJSON
        Name = 'select'
        OnReplyEvent = MasterEventsselectReplyEvent
      end
      item
        Routes = [crAll]
        DWParams = <>
        JsonMode = jmPureJSON
        Name = 'selectid'
        OnReplyEvent = MasterEventsselectidReplyEvent
      end
      item
        Routes = [crAll]
        DWParams = <>
        JsonMode = jmPureJSON
        Name = 'selectwhere'
        OnReplyEvent = MasterEventsselectwhereReplyEvent
      end
      item
        Routes = [crAll]
        DWParams = <>
        JsonMode = jmPureJSON
        Name = 'insert'
        OnReplyEventByType = MasterEventsinsertReplyEventByType
      end
      item
        Routes = [crAll]
        DWParams = <>
        JsonMode = jmPureJSON
        Name = 'update'
        OnReplyEventByType = MasterEventsupdateReplyEventByType
      end
      item
        Routes = [crAll]
        DWParams = <>
        JsonMode = jmPureJSON
        Name = 'delete'
        OnReplyEvent = MasterEventsdeleteReplyEvent
      end
      item
        Routes = [crAll]
        DWParams = <>
        JsonMode = jmPureJSON
        Name = 'nextpacket'
        OnReplyEvent = MasterEventsnextpacketReplyEvent
      end
      item
        Routes = [crAll]
        DWParams = <>
        JsonMode = jmPureJSON
        Name = 'nextpacketwhere'
        OnReplyEvent = MasterEventsnextpacketwhereReplyEvent
      end>
    ContextName = 'master'
    Left = 37
    Top = 158
  end
  object Lookup: TDWServerEvents
    IgnoreInvalidParams = False
    Events = <
      item
        Routes = [crAll]
        DWParams = <>
        JsonMode = jmPureJSON
        Name = 'select'
        OnReplyEventByType = LookupEventsselectReplyEventByType
      end>
    ContextName = 'lookup'
    Left = 204
    Top = 160
  end
end

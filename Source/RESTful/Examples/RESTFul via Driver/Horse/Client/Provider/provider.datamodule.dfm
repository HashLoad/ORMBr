object ProviderDM: TProviderDM
  OldCreateOrder = False
  Height = 279
  Width = 398
  object RESTClientHorse1: TRESTClientHorse
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Authenticator.AuthenticatorType = atNoAuth
    Protocol = Http
    Host = 'localhost'
    Port = 9000
    MethodGETId = 'selectid'
    MethodGETWhere = 'selectwhere'
    MethodGETNextPacket = 'nextpacket'
    MethodGETNextPacketWhere = 'nextpacketwhere'
    OnAuthentication = RESTClientHorse1Authentication
    OnBeforeCommand = RESTClientHorse1BeforeCommand
    OnAfterCommand = RESTClientHorse1AfterCommand
    OnErrorCommand = RESTClientHorse1ErrorCommand
    APIContext = 'api'
    RESTContext = '/ormbr'
    ORMBrServerUse = True
    Left = 66
    Top = 25
  end
  object FDMaster: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 236
    Top = 20
  end
  object FDDetail: TFDMemTable
    AggregatesActive = True
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 233
    Top = 73
  end
  object FDClient: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 231
    Top = 127
  end
  object FDLookup: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 231
    Top = 172
  end
end

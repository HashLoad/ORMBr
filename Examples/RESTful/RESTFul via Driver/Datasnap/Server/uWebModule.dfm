object WebModule1: TWebModule1
  OldCreateOrder = False
  OnCreate = WebModuleCreate
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
    end>
  Height = 333
  Width = 414
  object DSHTTPWebDispatcher1: TDSHTTPWebDispatcher
    Filters = <>
    WebDispatch.PathInfo = 'datasnap*'
    Left = 96
    Top = 137
  end
  object DSServer1: TDSServer
    Left = 96
    Top = 73
  end
  object Master: TDSServerClass
    OnGetClass = MasterGetClass
    OnPrepare = MasterPrepare
    Server = DSServer1
    LifeCycle = 'Server'
    Left = 202
    Top = 75
  end
  object Lookup: TDSServerClass
    OnGetClass = LookupGetClass
    Server = DSServer1
    LifeCycle = 'Server'
    Left = 282
    Top = 75
  end
end

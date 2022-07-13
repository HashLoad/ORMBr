object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Consumindo WebServer JSON'
  ClientHeight = 561
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    784
    561)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 81
    Top = 50
    Width = 33
    Height = 13
    Caption = 'Cidade'
  end
  object Label2: TLabel
    Left = 8
    Top = 50
    Width = 13
    Height = 13
    Caption = 'UF'
  end
  object Label3: TLabel
    Left = 303
    Top = 50
    Width = 52
    Height = 13
    Caption = 'Ocorr'#234'ncia'
  end
  object Label4: TLabel
    Left = 8
    Top = 394
    Width = 19
    Height = 13
    Caption = 'CEP'
  end
  object Label5: TLabel
    Left = 134
    Top = 394
    Width = 55
    Height = 13
    Caption = 'Logradouro'
  end
  object Label6: TLabel
    Left = 458
    Top = 394
    Width = 65
    Height = 13
    Caption = 'Complemento'
  end
  object Label7: TLabel
    Left = 134
    Top = 440
    Width = 33
    Height = 13
    Caption = 'Cidade'
  end
  object Label8: TLabel
    Left = 458
    Top = 440
    Width = 28
    Height = 13
    Caption = 'Bairro'
  end
  object Label9: TLabel
    Left = 8
    Top = 440
    Width = 13
    Height = 13
    Caption = 'UF'
  end
  object Label10: TLabel
    Left = 8
    Top = 486
    Width = 23
    Height = 13
    Caption = 'IBGE'
  end
  object Label11: TLabel
    Left = 134
    Top = 486
    Width = 39
    Height = 13
    Caption = 'Unidade'
  end
  object Label12: TLabel
    Left = 458
    Top = 486
    Width = 15
    Height = 13
    Caption = 'Gia'
  end
  object Label13: TLabel
    Left = 8
    Top = 4
    Width = 19
    Height = 13
    Caption = 'CEP'
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 106
    Width = 768
    Height = 282
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    ReadOnly = True
    TabOrder = 6
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object edtCidade: TEdit
    Left = 81
    Top = 69
    Width = 216
    Height = 21
    TabOrder = 4
    Text = 'Porto Alegre'
  end
  object edtUF: TComboBox
    Left = 8
    Top = 69
    Width = 67
    Height = 21
    Style = csDropDownList
    ItemIndex = 22
    TabOrder = 3
    Text = 'RS'
    Items.Strings = (
      'AC'
      'AL'
      'AM'
      'AP'
      'BA'
      'CE'
      'DF'
      'ES'
      'GO'
      'MA'
      'MG'
      'MS'
      'MT'
      'PA'
      'PB'
      'PE'
      'PI'
      'PR'
      'RJ'
      'RN'
      'RO'
      'RR'
      'RS'
      'SC'
      'SE'
      'SP'
      'TO')
  end
  object edtOcorrencia: TEdit
    Left = 303
    Top = 69
    Width = 216
    Height = 21
    TabOrder = 5
    Text = 'Domingos+Jose'
  end
  object btnBuscar: TButton
    Left = 525
    Top = 67
    Width = 75
    Height = 25
    Caption = 'Buscar'
    TabOrder = 2
    OnClick = btnBuscarClick
  end
  object DBEdit1: TDBEdit
    Left = 8
    Top = 413
    Width = 120
    Height = 21
    TabStop = False
    DataField = 'Cep'
    DataSource = DataSource1
    ReadOnly = True
    TabOrder = 7
  end
  object DBEdit2: TDBEdit
    Left = 134
    Top = 413
    Width = 318
    Height = 21
    TabStop = False
    DataField = 'Logradouro'
    DataSource = DataSource1
    ReadOnly = True
    TabOrder = 8
  end
  object DBEdit3: TDBEdit
    Left = 458
    Top = 413
    Width = 318
    Height = 21
    TabStop = False
    DataField = 'Logradouro'
    DataSource = DataSource1
    ReadOnly = True
    TabOrder = 9
  end
  object DBEdit4: TDBEdit
    Left = 134
    Top = 459
    Width = 318
    Height = 21
    TabStop = False
    DataField = 'Localidade'
    DataSource = DataSource1
    ReadOnly = True
    TabOrder = 11
  end
  object DBEdit5: TDBEdit
    Left = 458
    Top = 459
    Width = 318
    Height = 21
    TabStop = False
    DataField = 'Bairro'
    DataSource = DataSource1
    ReadOnly = True
    TabOrder = 12
  end
  object DBEdit6: TDBEdit
    Left = 8
    Top = 459
    Width = 120
    Height = 21
    TabStop = False
    DataField = 'UF'
    DataSource = DataSource1
    ReadOnly = True
    TabOrder = 10
  end
  object DBEdit7: TDBEdit
    Left = 8
    Top = 505
    Width = 120
    Height = 21
    TabStop = False
    DataField = 'IBGE'
    DataSource = DataSource1
    ReadOnly = True
    TabOrder = 13
  end
  object DBEdit8: TDBEdit
    Left = 134
    Top = 505
    Width = 318
    Height = 21
    TabStop = False
    DataField = 'Unidade'
    DataSource = DataSource1
    ReadOnly = True
    TabOrder = 14
  end
  object DBEdit9: TDBEdit
    Left = 458
    Top = 505
    Width = 318
    Height = 21
    TabStop = False
    DataField = 'Gia'
    DataSource = DataSource1
    ReadOnly = True
    TabOrder = 15
  end
  object DBNavigator1: TDBNavigator
    Left = 0
    Top = 536
    Width = 784
    Height = 25
    DataSource = DataSource1
    VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast]
    Align = alBottom
    TabOrder = 16
  end
  object edtCEPBusca: TEdit
    Left = 8
    Top = 23
    Width = 120
    Height = 21
    TabOrder = 1
  end
  object btnBuscaCEP: TButton
    Left = 134
    Top = 21
    Width = 75
    Height = 25
    Caption = 'Buscar'
    TabOrder = 0
    OnClick = btnBuscaCEPClick
  end
  object FDMemTable1: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 310
    Top = 196
  end
  object DataSource1: TDataSource
    AutoEdit = False
    DataSet = FDMemTable1
    Left = 310
    Top = 246
  end
  object RESTClientWS1: TRESTClientWS
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Protocol = Https
    Host = 'viacep.com.br/'
    Port = 0
    MethodGETId = 'selectid'
    MethodGETWhere = 'selectwhere'
    MethodGETNextPacket = 'nextpacket'
    MethodGETNextPacketWhere = 'nextpacketwhere'
    APIContext = 'ws'
    RESTContext = 'json'
    Left = 310
    Top = 142
  end
end

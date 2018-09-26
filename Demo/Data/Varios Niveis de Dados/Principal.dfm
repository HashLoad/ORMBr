object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 783
  ClientWidth = 689
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 689
    Height = 24
    Align = alTop
    Caption = 'Empresa'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 77
  end
  object Label2: TLabel
    Left = 0
    Top = 110
    Width = 689
    Height = 24
    Align = alTop
    Caption = 'Contato'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 70
  end
  object Label3: TLabel
    Left = 0
    Top = 220
    Width = 689
    Height = 24
    Align = alTop
    Caption = 'Contato Telefone'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 154
  end
  object Label4: TLabel
    Left = 0
    Top = 340
    Width = 689
    Height = 24
    Align = alTop
    Caption = 'Contato Rede Social'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 179
  end
  object Label5: TLabel
    Left = 0
    Top = 460
    Width = 689
    Height = 24
    Align = alTop
    Caption = 'Contato Email'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 125
  end
  object DBGrid1: TDBGrid
    AlignWithMargins = True
    Left = 3
    Top = 27
    Width = 683
    Height = 80
    Align = alTop
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBGrid2: TDBGrid
    AlignWithMargins = True
    Left = 3
    Top = 367
    Width = 683
    Height = 90
    Align = alTop
    DataSource = DataSource4
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBGrid3: TDBGrid
    AlignWithMargins = True
    Left = 3
    Top = 247
    Width = 683
    Height = 90
    Align = alTop
    DataSource = DataSource3
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBGrid4: TDBGrid
    AlignWithMargins = True
    Left = 3
    Top = 137
    Width = 683
    Height = 80
    Align = alTop
    DataSource = DataSource2
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Button1: TButton
    Left = 8
    Top = 755
    Width = 107
    Height = 25
    Caption = 'ApplyUpdates'
    TabOrder = 7
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 586
    Top = 755
    Width = 95
    Height = 25
    Caption = 'Monitor SQL'
    TabOrder = 9
    OnClick = Button2Click
  end
  object DBGrid5: TDBGrid
    AlignWithMargins = True
    Left = 3
    Top = 487
    Width = 683
    Height = 90
    Align = alTop
    DataSource = DataSource5
    TabOrder = 4
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 653
    Width = 683
    Height = 98
    Align = alTop
    Lines.Strings = (
      'Memo1')
    TabOrder = 6
  end
  object Button3: TButton
    Left = 272
    Top = 755
    Width = 121
    Height = 25
    Caption = 'Gerar JSON'
    TabOrder = 8
    OnClick = Button3Click
  end
  object DBGrid6: TDBGrid
    AlignWithMargins = True
    Left = 3
    Top = 583
    Width = 683
    Height = 64
    Align = alTop
    DataSource = DataSource6
    TabOrder = 5
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object FDMemTable1: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 316
    Top = 44
  end
  object FDMemTable2: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 316
    Top = 156
  end
  object FDMemTable3: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 306
    Top = 278
  end
  object FDMemTable4: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 304
    Top = 396
  end
  object DataSource1: TDataSource
    DataSet = FDMemTable1
    Left = 386
    Top = 44
  end
  object DataSource2: TDataSource
    DataSet = FDMemTable2
    Left = 382
    Top = 156
  end
  object DataSource3: TDataSource
    DataSet = FDMemTable3
    Left = 366
    Top = 278
  end
  object DataSource4: TDataSource
    DataSet = FDMemTable4
    Left = 364
    Top = 394
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      
        'Database=D:\ORMBr\Demo\Data\Varios Niveis de Dados\ORMBRTESTE.FD' +
        'B'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'CharacterSet=WIN1252'
      'DriverID=FB')
    LoginPrompt = False
    Left = 176
    Top = 52
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 174
    Top = 102
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    Left = 172
    Top = 156
  end
  object FDMemTable5: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 304
    Top = 514
  end
  object DataSource5: TDataSource
    DataSet = FDMemTable5
    Left = 364
    Top = 514
  end
  object FDMemTable6: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 298
    Top = 598
  end
  object DataSource6: TDataSource
    DataSet = FDMemTable6
    Left = 358
    Top = 598
  end
end

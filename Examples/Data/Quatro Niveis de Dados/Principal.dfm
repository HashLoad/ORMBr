object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 638
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
    Caption = 'N'#237'vel 1 (Atendimento)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 197
  end
  object Label2: TLabel
    Left = 0
    Top = 150
    Width = 689
    Height = 24
    Align = alTop
    Caption = 'N'#237'vel 2 (Exame)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 143
  end
  object Label3: TLabel
    Left = 0
    Top = 300
    Width = 689
    Height = 24
    Align = alTop
    Caption = 'N'#237'vel 3 (Procedimento)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 205
  end
  object Label4: TLabel
    Left = 0
    Top = 450
    Width = 689
    Height = 24
    Align = alTop
    Caption = 'N'#237'vel 4 (Setor)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 130
  end
  object DBGrid1: TDBGrid
    AlignWithMargins = True
    Left = 3
    Top = 27
    Width = 683
    Height = 120
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
    Top = 477
    Width = 683
    Height = 120
    Align = alTop
    DataSource = DataSource4
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBGrid3: TDBGrid
    AlignWithMargins = True
    Left = 3
    Top = 327
    Width = 683
    Height = 120
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
    Top = 177
    Width = 683
    Height = 120
    Align = alTop
    DataSource = DataSource2
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Button1: TButton
    Left = 10
    Top = 603
    Width = 107
    Height = 25
    Caption = 'ApplyUpdates'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 586
    Top = 603
    Width = 95
    Height = 25
    Caption = 'Monitor SQL'
    TabOrder = 5
    OnClick = Button2Click
  end
  object FDMemTable1: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 326
    Top = 64
  end
  object FDMemTable2: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 324
    Top = 216
  end
  object FDMemTable3: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 322
    Top = 362
  end
  object FDMemTable4: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 320
    Top = 510
  end
  object DataSource1: TDataSource
    DataSet = FDMemTable1
    Left = 396
    Top = 64
  end
  object DataSource2: TDataSource
    DataSet = FDMemTable2
    Left = 390
    Top = 216
  end
  object DataSource3: TDataSource
    DataSet = FDMemTable3
    Left = 382
    Top = 362
  end
  object DataSource4: TDataSource
    DataSet = FDMemTable4
    Left = 380
    Top = 510
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=.\NIVEL4.FDB'
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
end

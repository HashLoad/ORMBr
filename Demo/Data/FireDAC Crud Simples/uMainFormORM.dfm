object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 467
  ClientWidth = 864
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 12
    Top = 346
    Width = 41
    Height = 13
    Caption = 'Client ID'
  end
  object Label4: TLabel
    Left = 144
    Top = 346
    Width = 83
    Height = 13
    Caption = 'Client Description'
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 4
    Width = 848
    Height = 333
    DataSource = DataSource3
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBNavigator1: TDBNavigator
    Left = 8
    Top = 406
    Width = 840
    Height = 25
    DataSource = DataSource3
    TabOrder = 3
  end
  object Button2: TButton
    Left = 8
    Top = 437
    Width = 113
    Height = 25
    Caption = 'ApplyUpdates'
    TabOrder = 4
    OnClick = Button2Click
  end
  object DBEdit3: TDBEdit
    Left = 12
    Top = 362
    Width = 121
    Height = 21
    DataField = 'client_id'
    DataSource = DataSource3
    TabOrder = 1
  end
  object DBEdit4: TDBEdit
    Left = 144
    Top = 362
    Width = 601
    Height = 21
    DataField = 'client_name'
    DataSource = DataSource3
    Enabled = False
    TabOrder = 2
  end
  object Button1: TButton
    Left = 132
    Top = 437
    Width = 75
    Height = 25
    Caption = 'Object Post'
    TabOrder = 5
    OnClick = Button1Click
  end
  object DataSource3: TDataSource
    DataSet = FDClient
    Left = 426
    Top = 168
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=..\Database\database.db3'
      'DriverID=SQLite')
    LoginPrompt = False
    Left = 154
    Top = 42
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 218
    Top = 42
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 86
    Top = 42
  end
  object FDClient: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 354
    Top = 168
  end
end

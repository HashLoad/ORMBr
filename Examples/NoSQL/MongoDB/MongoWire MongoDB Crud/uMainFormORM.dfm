object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 591
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
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 858
    Height = 219
    Align = alTop
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
  object DBGrid2: TDBGrid
    Left = 0
    Top = 225
    Width = 864
    Height = 172
    Align = alTop
    DataSource = DataSource1
    TabOrder = 6
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 468
    Width = 858
    Height = 120
    Align = alBottom
    Lines.Strings = (
      'Memo1')
    TabOrder = 7
  end
  object DataSource3: TDataSource
    DataSet = CDSClient
    Left = 426
    Top = 168
  end
  object CDSClient: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 354
    Top = 168
  end
  object DataSource1: TDataSource
    Left = 428
    Top = 238
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 788
    Top = 32
  end
  object MongoWireConnection1: TMongoWireConnection
    Database = 'database'
    Host = 'localhost'
    Port = 27017
    Connected = False
    Authenticate = False
    Left = 81
    Top = 60
  end
end

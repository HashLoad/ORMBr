object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 469
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
  object Label1: TLabel
    Left = 12
    Top = 132
    Width = 47
    Height = 13
    Caption = 'Master ID'
  end
  object Label2: TLabel
    Left = 144
    Top = 132
    Width = 89
    Height = 13
    Caption = 'Master Description'
  end
  object Label3: TLabel
    Left = 12
    Top = 174
    Width = 41
    Height = 13
    Caption = 'Client ID'
  end
  object Label4: TLabel
    Left = 144
    Top = 174
    Width = 83
    Height = 13
    Caption = 'Client Description'
  end
  object Label5: TLabel
    Left = 12
    Top = 218
    Width = 76
    Height = 13
    Caption = 'Master Register'
  end
  object Label6: TLabel
    Left = 144
    Top = 218
    Width = 71
    Height = 13
    Caption = 'Master Update'
  end
  object Label7: TLabel
    Left = 8
    Top = 265
    Width = 33
    Height = 13
    Caption = 'Detail'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label8: TLabel
    Left = 668
    Top = 383
    Width = 61
    Height = 13
    Caption = 'Total Pre'#231'o :'
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 4
    Width = 848
    Height = 121
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBNavigator1: TDBNavigator
    Left = 8
    Top = 405
    Width = 840
    Height = 25
    DataSource = DataSource1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 8
    Top = 436
    Width = 113
    Height = 25
    Caption = 'ApplyUpdates'
    TabOrder = 2
    OnClick = Button2Click
  end
  object DBGrid2: TDBGrid
    Left = 8
    Top = 279
    Width = 848
    Height = 96
    DataSource = DataSource2
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBEdit1: TDBEdit
    Left = 12
    Top = 148
    Width = 121
    Height = 21
    DataField = 'master_id'
    DataSource = DataSource1
    TabOrder = 4
  end
  object DBEdit2: TDBEdit
    Left = 144
    Top = 148
    Width = 712
    Height = 21
    DataField = 'description'
    DataSource = DataSource1
    TabOrder = 5
  end
  object DBEdit3: TDBEdit
    Left = 12
    Top = 190
    Width = 121
    Height = 21
    DataField = 'client_id'
    DataSource = DataSource1
    TabOrder = 6
  end
  object DBEdit4: TDBEdit
    Left = 144
    Top = 190
    Width = 597
    Height = 21
    DataField = 'client_name'
    DataSource = DataSource3
    ReadOnly = True
    TabOrder = 7
  end
  object DBEdit5: TDBEdit
    Left = 12
    Top = 234
    Width = 121
    Height = 21
    DataField = 'registerdate'
    DataSource = DataSource1
    TabOrder = 8
  end
  object DBEdit6: TDBEdit
    Left = 144
    Top = 234
    Width = 121
    Height = 21
    DataField = 'updatedate'
    DataSource = DataSource1
    TabOrder = 9
  end
  object DBEdit7: TDBEdit
    Left = 735
    Top = 379
    Width = 121
    Height = 21
    DataField = 'AGGPRICE'
    DataSource = DataSource2
    TabOrder = 10
  end
  object DBImage1: TDBImage
    Left = 751
    Top = 172
    Width = 105
    Height = 105
    BorderStyle = bsNone
    Ctl3D = True
    DataField = 'client_foto'
    ParentColor = True
    ParentCtl3D = False
    TabOrder = 11
  end
  object Button1: TButton
    Left = 735
    Top = 436
    Width = 113
    Height = 25
    Caption = 'Monitor'
    TabOrder = 12
    OnClick = Button1Click
  end
  object DataSource1: TDataSource
    DataSet = Master
    Left = 424
    Top = 60
  end
  object Detail: TClientDataSet
    Aggregates = <>
    AggregatesActive = True
    Params = <>
    Left = 330
    Top = 106
  end
  object DataSource2: TDataSource
    DataSet = Detail
    Left = 422
    Top = 112
  end
  object Client: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 334
    Top = 166
  end
  object DataSource3: TDataSource
    AutoEdit = False
    DataSet = Client
    Left = 426
    Top = 168
  end
  object Master: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 328
    Top = 56
  end
  object Lookup: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 334
    Top = 220
  end
  object IBDatabase1: TIBDatabase
    DatabaseName = 'D:\ORMBr\Demo\Data\Database\database.fdb'
    Params.Strings = (
      'user_name=SYSDBA'
      'password=masterkey')
    LoginPrompt = False
    ServerType = 'IBServer'
    Left = 120
    Top = 45
  end
end

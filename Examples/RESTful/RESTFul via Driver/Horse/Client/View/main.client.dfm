object FormClient: TFormClient
  Left = 0
  Top = 0
  Caption = 'FormClient'
  ClientHeight = 628
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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 166
    Width = 47
    Height = 13
    Caption = 'Master ID'
  end
  object Label2: TLabel
    Left = 144
    Top = 166
    Width = 89
    Height = 13
    Caption = 'Master Description'
  end
  object Label3: TLabel
    Left = 12
    Top = 208
    Width = 41
    Height = 13
    Caption = 'Client ID'
  end
  object Label4: TLabel
    Left = 144
    Top = 208
    Width = 83
    Height = 13
    Caption = 'Client Description'
  end
  object Label5: TLabel
    Left = 12
    Top = 252
    Width = 76
    Height = 13
    Caption = 'Master Register'
  end
  object Label6: TLabel
    Left = 144
    Top = 252
    Width = 71
    Height = 13
    Caption = 'Master Update'
  end
  object Label7: TLabel
    Left = 8
    Top = 299
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
    Top = 417
    Width = 61
    Height = 13
    Caption = 'Total Pre'#231'o :'
  end
  object Label9: TLabel
    Left = 8
    Top = 512
    Width = 83
    Height = 13
    Caption = 'Resposta do Ping'
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 4
    Width = 848
    Height = 156
    DataSource = dtsMaster
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBNavigator1: TDBNavigator
    Left = 8
    Top = 440
    Width = 840
    Height = 25
    DataSource = dtsMaster
    TabOrder = 10
  end
  object Button2: TButton
    Left = 8
    Top = 471
    Width = 97
    Height = 25
    Caption = 'ApplyUpdates'
    TabOrder = 11
    OnClick = Button2Click
  end
  object DBGrid2: TDBGrid
    Left = 8
    Top = 313
    Width = 848
    Height = 96
    DataSource = dtsDetail
    TabOrder = 8
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBEdit1: TDBEdit
    Left = 12
    Top = 182
    Width = 121
    Height = 21
    DataField = 'master_id'
    DataSource = dtsMaster
    TabOrder = 1
  end
  object DBEdit2: TDBEdit
    Left = 144
    Top = 182
    Width = 712
    Height = 21
    DataField = 'description'
    DataSource = dtsMaster
    TabOrder = 2
  end
  object DBEdit3: TDBEdit
    Left = 12
    Top = 224
    Width = 121
    Height = 21
    DataField = 'client_id'
    DataSource = dtsMaster
    TabOrder = 4
  end
  object DBEdit4: TDBEdit
    Left = 144
    Top = 224
    Width = 601
    Height = 21
    DataField = 'client_name'
    DataSource = dtsClient
    Enabled = False
    TabOrder = 5
  end
  object DBEdit5: TDBEdit
    Left = 12
    Top = 268
    Width = 121
    Height = 21
    DataField = 'registerdate'
    DataSource = dtsMaster
    TabOrder = 6
  end
  object DBEdit6: TDBEdit
    Left = 144
    Top = 268
    Width = 121
    Height = 21
    DataField = 'updatedate'
    DataSource = dtsMaster
    TabOrder = 7
  end
  object DBEdit7: TDBEdit
    Left = 735
    Top = 413
    Width = 121
    Height = 21
    DataField = 'AGGPRICE'
    DataSource = dtsDetail
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 9
  end
  object DBImage1: TDBImage
    Left = 751
    Top = 206
    Width = 105
    Height = 105
    Ctl3D = True
    DataField = 'client_foto'
    DataSource = dtsClient
    ParentColor = True
    ParentCtl3D = False
    Stretch = True
    TabOrder = 3
  end
  object Button3: TButton
    Left = 111
    Top = 471
    Width = 97
    Height = 25
    Caption = 'Monitor'
    TabOrder = 12
    OnClick = Button3Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 531
    Width = 737
    Height = 89
    Lines.Strings = (
      'Memo1')
    TabOrder = 13
  end
  object Button4: TButton
    Left = 751
    Top = 531
    Width = 97
    Height = 25
    Caption = 'Ping'
    TabOrder = 14
    OnClick = Button4Click
  end
  object dtsMaster: TDataSource
    Left = 424
    Top = 60
  end
  object dtsDetail: TDataSource
    Left = 413
    Top = 344
  end
  object dtsClient: TDataSource
    AutoEdit = False
    Left = 420
    Top = 220
  end
end

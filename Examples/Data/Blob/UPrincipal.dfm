object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 648
  ClientWidth = 635
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
    Left = 8
    Top = 156
    Width = 76
    Height = 13
    Caption = 'PERSON_FLD11'
  end
  object Label2: TLabel
    Left = 3
    Top = 286
    Width = 76
    Height = 13
    Caption = 'PERSON_FLD12'
  end
  object Label3: TLabel
    Left = 3
    Top = 438
    Width = 76
    Height = 13
    Caption = 'PERSON_FLD13'
  end
  object Image1: TImage
    Left = 458
    Top = 175
    Width = 105
    Height = 105
  end
  object DBGrid1: TDBGrid
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 629
    Height = 142
    Align = alTop
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBImage1: TDBImage
    Left = 3
    Top = 175
    Width = 105
    Height = 105
    DataField = 'PERSON_FLD11'
    DataSource = DataSource1
    TabOrder = 1
  end
  object DBMemo1: TDBMemo
    Left = 3
    Top = 305
    Width = 629
    Height = 126
    DataField = 'PERSON_FLD12'
    DataSource = DataSource1
    TabOrder = 2
  end
  object DBMemo2: TDBMemo
    Left = 3
    Top = 457
    Width = 629
    Height = 126
    DataField = 'PERSON_FLD13'
    DataSource = DataSource1
    TabOrder = 3
  end
  object Button1: TButton
    Left = 3
    Top = 589
    Width = 75
    Height = 25
    Caption = 'Load XML'
    TabOrder = 4
    OnClick = Button1Click
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      
        'Database=C:\workspace\Delphi\Frameworks\ORMBr\Examples\Data\Blob' +
        '\blob.fdb'
      'Password=masterkey'
      'User_Name=SYSDBA'
      'CharacterSet=WIN1252'
      'DriverID=FB')
    LoginPrompt = False
    Left = 46
    Top = 22
  end
  object DataSource1: TDataSource
    DataSet = FDMemTable1
    Left = 213
    Top = 24
  end
  object FDMemTable1: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 138
    Top = 23
  end
  object OpenDialog1: TOpenDialog
    Left = 252
    Top = 588
  end
end

object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 376
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    527
    376)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 104
    Top = 8
    Width = 145
    Height = 25
    Caption = 'Executar FindWhere()'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 47
    Width = 527
    Height = 329
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Button2: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Monitor'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 271
    Top = 8
    Width = 145
    Height = 25
    Caption = 'Executar Lazy()'
    TabOrder = 3
    OnClick = Button3Click
  end
  object FDConnection: TFDConnection
    Params.Strings = (
      
        'Database=D:\PROJETOS-Brasil\ORMBr\Examples\Data\Object Lazy\TEST' +
        'E.FDB'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'Server=localhost'
      'DriverID=FB')
    LoginPrompt = False
    Left = 237
    Top = 122
  end
end

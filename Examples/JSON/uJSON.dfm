object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Form4'
  ClientHeight = 472
  ClientWidth = 830
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object Button1: TButton
    Left = 4
    Top = 8
    Width = 151
    Height = 73
    Caption = 'ObjectToJson'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 94
    Width = 830
    Height = 378
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
    ExplicitWidth = 824
    ExplicitHeight = 369
  end
  object Button2: TButton
    Left = 161
    Top = 8
    Width = 144
    Height = 73
    Caption = 'JsonToObject'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 311
    Top = 8
    Width = 144
    Height = 73
    Caption = 'JSON Object'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 461
    Top = 8
    Width = 144
    Height = 73
    Caption = 'JSON Array'
    TabOrder = 4
    OnClick = Button4Click
  end
end

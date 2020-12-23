object FormPrincipal: TFormPrincipal
  Left = 0
  Top = 0
  Caption = 'FormPrincipal'
  ClientHeight = 481
  ClientWidth = 730
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
  object LabelID: TLabel
    Left = 345
    Top = 69
    Width = 36
    Height = 13
    Caption = 'LabelID'
  end
  object LabelPreco: TLabel
    Left = 345
    Top = 123
    Width = 36
    Height = 13
    Caption = 'LabelID'
  end
  object EditID: TEdit
    Left = 345
    Top = 84
    Width = 64
    Height = 21
    TabOrder = 0
    Text = 'EditID'
  end
  object EditPreco: TEdit
    Left = 345
    Top = 138
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'EditPreco'
  end
  object Button1: TButton
    Left = 420
    Top = 82
    Width = 115
    Height = 25
    Caption = 'Mostrar (Produto.ID)'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 472
    Top = 136
    Width = 135
    Height = 25
    Caption = 'Mostrar (Produto.Preco)'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 345
    Top = 187
    Width = 262
    Height = 25
    Caption = 'Alterar (Produto.ID e Produto.Preco)'
    TabOrder = 4
    OnClick = Button3Click
  end
  object ComboEditID: TComboBox
    Left = 147
    Top = 84
    Width = 150
    Height = 21
    TabOrder = 5
    Text = 'ComboEditID'
    Items.Strings = (
      '0 - Zero'
      '1 - Um'
      '2 - Dois'
      '3 - Tr'#234's'
      '4 - Quatro'
      '5 - Cinco'
      '6 - Seis'
      '7 - Sete'
      '8 - Oito'
      '9 - Nove'
      '10 - Dez')
  end
  object EditSoma: TEdit
    Left = 345
    Top = 234
    Width = 121
    Height = 21
    TabOrder = 6
    Text = 'EditSoma'
  end
  object ProgressBarID: TProgressBar
    Left = 147
    Top = 123
    Width = 150
    Height = 17
    Max = 10
    TabOrder = 7
  end
  object Button4: TButton
    Left = 472
    Top = 232
    Width = 135
    Height = 25
    Caption = 'Mostrar (Produto.Soma)'
    TabOrder = 8
    OnClick = Button4Click
  end
end

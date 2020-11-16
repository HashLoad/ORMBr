object frmORMBrDependencies: TfrmORMBrDependencies
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'frmORMBrDependencies'
  ClientHeight = 382
  ClientWidth = 681
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
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 681
    Height = 81
    Align = alTop
    BevelOuter = bvNone
    Color = 4864316
    ParentBackground = False
    TabOrder = 0
    object Label1: TLabel
      Left = 14
      Top = 19
      Width = 410
      Height = 19
      Caption = 'Assistente de download das depend'#234'ncias do ORMBr'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object Label2: TLabel
      Left = 14
      Top = 41
      Width = 129
      Height = 13
      Cursor = crHandPoint
      Caption = 'http://www.ormbr.com.br/'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 341
    Width = 681
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    Color = 4864316
    ParentBackground = False
    TabOrder = 1
    ExplicitTop = 312
    ExplicitWidth = 675
    object btnCancel: TButton
      Left = 560
      Top = 8
      Width = 99
      Height = 25
      Caption = 'Cancelar'
      ModalResult = 8
      TabOrder = 0
      OnClick = btnCancelClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 81
    Width = 681
    Height = 260
    Align = alClient
    BevelOuter = bvNone
    Color = 3417897
    ParentBackground = False
    TabOrder = 2
    ExplicitTop = 312
    ExplicitWidth = 675
    ExplicitHeight = 41
    object vlDependencies: TValueListEditor
      Left = 0
      Top = 0
      Width = 681
      Height = 260
      Align = alClient
      Color = 3417897
      DrawingStyle = gdsGradient
      FixedColor = 3417897
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      GradientEndColor = 3417897
      GradientStartColor = 3417897
      ParentFont = False
      TabOrder = 0
      TitleCaptions.Strings = (
        'Reposit'#243'rio'
        'Tag')
      ExplicitTop = 22
      ExplicitWidth = 385
      ExplicitHeight = 219
      ColWidths = (
        194
        481)
    end
  end
end

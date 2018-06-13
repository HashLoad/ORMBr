object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 452
  ClientWidth = 864
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label7: TLabel
    Left = 12
    Top = 301
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
  object Label1: TLabel
    Left = 12
    Top = 168
    Width = 47
    Height = 13
    Caption = 'Master ID'
  end
  object Label2: TLabel
    Left = 144
    Top = 168
    Width = 89
    Height = 13
    Caption = 'Master Description'
  end
  object Label3: TLabel
    Left = 12
    Top = 210
    Width = 41
    Height = 13
    Caption = 'Client ID'
  end
  object Label4: TLabel
    Left = 144
    Top = 210
    Width = 83
    Height = 13
    Caption = 'Client Description'
  end
  object Label5: TLabel
    Left = 12
    Top = 254
    Width = 76
    Height = 13
    Caption = 'Master Register'
  end
  object Label6: TLabel
    Left = 144
    Top = 254
    Width = 71
    Height = 13
    Caption = 'Master Update'
  end
  object Label8: TLabel
    Left = 668
    Top = 419
    Width = 61
    Height = 13
    Caption = 'Total Pre'#231'o :'
  end
  object imgClient_Foto: TImage
    Left = 751
    Top = 209
    Width = 105
    Height = 105
  end
  object btnOpen: TButton
    Left = 12
    Top = 419
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 10
    OnClick = btnOpenClick
  end
  object StringGridMaster: TStringGrid
    Left = 12
    Top = 6
    Width = 844
    Height = 120
    ColCount = 6
    DefaultRowHeight = 20
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected]
    TabOrder = 0
    OnSelectCell = StringGridMasterSelectCell
    ColWidths = (
      64
      64
      64
      64
      64
      64)
    RowHeights = (
      20)
  end
  object StringGridDetail: TStringGrid
    Left = 12
    Top = 316
    Width = 844
    Height = 93
    DefaultRowHeight = 20
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    TabOrder = 8
    ColWidths = (
      64
      64
      64
      64
      64)
    RowHeights = (
      20)
  end
  object edtMaster_Descricao: TEdit
    Left = 144
    Top = 184
    Width = 712
    Height = 21
    TabOrder = 3
  end
  object edtClient_ID: TEdit
    Left = 12
    Top = 226
    Width = 121
    Height = 21
    NumbersOnly = True
    TabOrder = 4
  end
  object edtClient_Nome: TEdit
    Left = 144
    Top = 226
    Width = 591
    Height = 21
    TabOrder = 5
  end
  object edtMaster_Cadastro: TEdit
    Left = 12
    Top = 270
    Width = 121
    Height = 21
    TabOrder = 6
  end
  object edtMaster_Alteracao: TEdit
    Left = 144
    Top = 270
    Width = 121
    Height = 21
    TabOrder = 7
  end
  object Edit7: TEdit
    Left = 735
    Top = 415
    Width = 121
    Height = 21
    Alignment = taRightJustify
    TabOrder = 9
  end
  object Button1: TButton
    Left = 13
    Top = 132
    Width = 155
    Height = 25
    Caption = 'Proximo Pacote de Registros'
    TabOrder = 1
    OnClick = Button1Click
  end
  object edtMaster_ID: TEdit
    Left = 12
    Top = 184
    Width = 121
    Height = 21
    NumbersOnly = True
    TabOrder = 2
  end
  object btnInsert: TButton
    Left = 282
    Top = 419
    Width = 75
    Height = 25
    Caption = 'Insert'
    TabOrder = 11
    OnClick = btnInsertClick
  end
  object btnDelete: TButton
    Left = 444
    Top = 419
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 12
    OnClick = btnDeleteClick
  end
  object btnUpdate: TButton
    Left = 363
    Top = 419
    Width = 75
    Height = 25
    Caption = 'Update'
    TabOrder = 13
    OnClick = btnUpdateClick
  end
  object Button2: TButton
    Left = 93
    Top = 419
    Width = 115
    Height = 25
    Caption = 'Show SQLMonitor'
    TabOrder = 14
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 542
    Top = 419
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 15
    OnClick = Button3Click
  end
  object ZConnection1: TZConnection
    ControlsCodePage = cCP_UTF16
    Port = 0
    Database = '..\Database\database.db3'
    Protocol = 'sqlite-3'
    Left = 56
    Top = 46
  end
end

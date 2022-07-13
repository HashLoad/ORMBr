object framePacotes: TframePacotes
  Left = 0
  Top = 0
  Width = 545
  Height = 525
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Color = 3417897
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  object pnlBotoesMarcar: TPanel
    Left = 0
    Top = 484
    Width = 545
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnPacotesDesmarcarTodos: TSpeedButton
      AlignWithMargins = True
      Left = 495
      Top = 3
      Width = 47
      Height = 35
      Hint = 'Desmarcar todos os pacotes'
      Margins.Left = 0
      Align = alRight
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFD4D6E6262E83262E83D4D6E6FFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFD4D6E6262F82252D7FD4D6E6FFFFFFFFFFFFD4D6E6293284
        4F57C5313BBC262E83D4D6E6FFFFFFFFFFFFFFFFFFD4D6E6262F82343CAE343C
        AE262F82D4D6E6FFFFFF2A33855B63CC696BDE5A5DDA2B35BB262E83D4D6E6FF
        FFFFD4D6E6262F83323AB1393FD8393FD7343CAE262F82FFFFFF252D7F535CC1
        6D70E06A69DF4B4ED72A34BC262E83E3E4EE262F832F38B5373DD9383DE8383E
        D82D35A1252D7FFFFFFFD4D6E62A33855D66CF6B6FE06968DF3F42D52934BD19
        22802C36BA363BD7383BE4373DD92B33A4252D7FD4D6E6FFFFFFFFFFFFD4D6E6
        2932855C64CF6A6DE06261DD3639D42D38CF3439D63739DF353BD82832A8252D
        7FD4D6E6FFFFFFFFFFFFFFFFFFFFFFFFD4D6E62932855A63D0686CE05857DB35
        36D63737D93338D72630AB252D7FD4D6E6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFA8ACCC212A846972E6676ADF4C4BD83536D62A35CC17207BA8ACCCFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4D6E62A3386636CD66875E3656EDE62
        65DE3F3ED63236D52732C2262E84D4D6E6FFFFFFFFFFFFFFFFFFFFFFFFD4D6E6
        2B34866E76DA7081E66578DF6773E3646DE35A5EDE3938D43235D52631C3262E
        84D4D6E6FFFFFFFFFFFFD4D6E62C35867981DE7F8FEA7186E26E7FE65F68CA28
        31855C65D74C50DC3635D43135D62531C4262E84D4D6E6FFFFFF2D3586848CE1
        909DEE8295E67E8DEA6A72CF2A3381C5C8DD464E96515BD33E43D93635D43135
        D62430C5262E84FFFFFF2D35868990E297A4F08E9CEE767DCC2B3482D4D6E6FF
        FFFFE3E4EE2932854C56D32F35D82F34D7232FC7262E84FFFFFFD4D6E62D3586
        878FE38288CA2C3582D4D6E6FFFFFFFFFFFFFFFFFFD4D6E6283285404BD21F2D
        CC252E84D4D6E6FFFFFFFFFFFFD4D6E62D35832D3583D4D6E6FFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFD4D6E6283185283185D4D6E6FFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      OnClick = btnPacotesDesmarcarTodosClick
      ExplicitLeft = 677
    end
    object btnPacotesMarcarTodos: TSpeedButton
      AlignWithMargins = True
      Left = 445
      Top = 3
      Width = 47
      Height = 35
      Hint = 'Marcar todos os pacotes'
      Align = alRight
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFDFDFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE6ECE644915F7E
        A384FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFE9EEE9418D5517DA5E1EC05A82A686FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFECF0EC448D5314BD4F10C64D0F
        C54C1CA94E88AE8BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        EDF2ED4D945738B55D35BD5D35BE6133BE5E2DB5572F9E518EB590FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFF0F4F05096564EB06450B96750BA6958965F48
        9F5A51BD6A52B8695DAC6B93BA94FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9EC29E
        59AC6664B87163BA7263A167F4F7F4C3D6C454A66065BC7464B6705DA86697BE
        97FFFFFFFFFFFFFFFFFFFFFFFFFBFCFB6AAA6C74BD7A64A865F2F6F2FFFFFFFF
        FFFFC0D7C05DAD627AC18079BD7E5FAA6299C299FFFFFFFFFFFFFFFFFFFFFFFF
        FAFBFAB1D1B1F1F5F1FFFFFFFFFFFFFFFFFFFFFFFFBAD5BA66B56790CF918FCC
        9066B566B8D6B8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFB4D4B470C170A8E5A858B258D6E7D6FFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAED1AE81C4
        81C9E0C9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFEFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      OnClick = btnPacotesMarcarTodosClick
      ExplicitLeft = 627
    end
  end
  object ScrollBox1: TScrollBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 539
    Height = 478
    Align = alClient
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 0
    object Label6: TLabel
      Left = 12
      Top = 7
      Width = 74
      Height = 13
      Caption = 'Wizard/Core '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Bevel2: TBevel
      Left = 197
      Top = 12
      Width = 330
      Height = 14
      Shape = bsTopLine
    end
    object Label13: TLabel
      Left = 197
      Top = 435
      Width = 184
      Height = 26
      Margins.Left = 5
      Caption = 
        'Conex'#227'o ao MongoDB via MongoWire (https://github.com/torodb/mong' +
        'owp)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Label1: TLabel
      Left = 34
      Top = 435
      Width = 146
      Height = 13
      Caption = 'DBEBr Connection Mongo Wire'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 34
      Top = 27
      Width = 76
      Height = 13
      Caption = 'ORMBr Register'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 197
      Top = 29
      Width = 276
      Height = 13
      Margins.Left = 5
      Caption = 'Registrar o ORMBr Framework for Delphi na IDE do Delphi'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel
      Left = 12
      Top = 102
      Width = 114
      Height = 13
      Caption = 'ORMBr Drivers Links'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Bevel1: TBevel
      Left = 197
      Top = 113
      Width = 330
      Height = 14
      Shape = bsTopLine
    end
    object Label8: TLabel
      Left = 34
      Top = 122
      Width = 96
      Height = 13
      Caption = 'ORMBr Drivers Links'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label9: TLabel
      Left = 197
      Top = 121
      Width = 296
      Height = 26
      Margins.Left = 5
      Caption = 
        'Componentes para adicionarar as units do banco selecionado autom' +
        'aticamente ao seu projeto.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Label10: TLabel
      Left = 12
      Top = 235
      Width = 179
      Height = 13
      Caption = 'DBEBr Components Connections'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label11: TLabel
      Left = 34
      Top = 255
      Width = 131
      Height = 13
      Caption = 'DBEBr Connection FireDAC '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label12: TLabel
      Left = 197
      Top = 255
      Width = 199
      Height = 13
      Margins.Left = 5
      Caption = 'Componentes para conex'#227'o com FireDAC'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label15: TLabel
      Left = 34
      Top = 276
      Width = 140
      Height = 13
      Caption = 'DBEBr Connection DBExpress'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label16: TLabel
      Left = 197
      Top = 276
      Width = 211
      Height = 13
      Margins.Left = 5
      Caption = 'Componentes para conex'#227'o com DBExpress'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label17: TLabel
      Left = 34
      Top = 297
      Width = 112
      Height = 13
      Caption = 'DBEBr Connection Zeos'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label18: TLabel
      Left = 197
      Top = 297
      Width = 183
      Height = 13
      Margins.Left = 5
      Caption = 'Componentes para conex'#227'o com Zeos'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label14: TLabel
      Left = 34
      Top = 317
      Width = 125
      Height = 13
      Caption = 'DBEBr Connection UniDAC'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label19: TLabel
      Left = 197
      Top = 317
      Width = 196
      Height = 13
      Margins.Left = 5
      Caption = 'Componentes para conex'#227'o com UniDAC'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label20: TLabel
      Left = 34
      Top = 337
      Width = 124
      Height = 13
      Caption = 'DBEBr Connection FIBPlus'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label21: TLabel
      Left = 197
      Top = 337
      Width = 195
      Height = 13
      Margins.Left = 5
      Caption = 'Componentes para conex'#227'o com FIBPlus'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label22: TLabel
      Left = 34
      Top = 357
      Width = 136
      Height = 13
      Caption = 'DBEBr Connection SQLDirect'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label23: TLabel
      Left = 197
      Top = 357
      Width = 207
      Height = 13
      Margins.Left = 5
      Caption = 'Componentes para conex'#227'o com SQLDirect'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label24: TLabel
      Left = 34
      Top = 377
      Width = 131
      Height = 13
      Caption = 'DBEBr Connection IBObject'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label25: TLabel
      Left = 197
      Top = 377
      Width = 202
      Height = 13
      Margins.Left = 5
      Caption = 'Componentes para conex'#227'o com IBObject'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label26: TLabel
      Left = 34
      Top = 397
      Width = 132
      Height = 13
      Caption = 'DBEBr Connection NexusDB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label27: TLabel
      Left = 197
      Top = 397
      Width = 203
      Height = 13
      Margins.Left = 5
      Caption = 'Componentes para conex'#227'o com NexusDB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label28: TLabel
      Left = 34
      Top = 417
      Width = 111
      Height = 13
      Caption = 'DBEBr Connection ADO'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label29: TLabel
      Left = 197
      Top = 417
      Width = 182
      Height = 13
      Margins.Left = 5
      Caption = 'Componentes para conex'#227'o com ADO'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 34
      Top = 78
      Width = 59
      Height = 13
      Caption = 'ORMBr Core'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 197
      Top = 80
      Width = 179
      Height = 13
      Margins.Left = 5
      Caption = 'Core do ORMBr Framework for Delphi'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label30: TLabel
      Left = 34
      Top = 44
      Width = 55
      Height = 13
      Caption = 'DBEBr Core'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label31: TLabel
      Left = 197
      Top = 46
      Width = 248
      Height = 13
      Margins.Left = 5
      Caption = 'Core do DBEBr Framework for Delphi (Depend'#234'ncia)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Bevel3: TBevel
      Left = 197
      Top = 236
      Width = 330
      Height = 14
      Shape = bsTopLine
    end
    object Label33: TLabel
      Left = 34
      Top = 171
      Width = 153
      Height = 13
      Caption = 'ORMBr Manager TClientDataSet'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label34: TLabel
      Left = 197
      Top = 171
      Width = 171
      Height = 13
      Margins.Left = 5
      Caption = 'Componentes do TManagerDataSet'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label35: TLabel
      Left = 12
      Top = 152
      Width = 168
      Height = 13
      Caption = 'ORMBr Components  Manager'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Bevel5: TBevel
      Left = 197
      Top = 156
      Width = 330
      Height = 14
      Shape = bsTopLine
    end
    object Label36: TLabel
      Left = 34
      Top = 190
      Width = 148
      Height = 13
      Caption = 'ORMBr Manager TFDMemTable'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label37: TLabel
      Left = 197
      Top = 190
      Width = 171
      Height = 13
      Margins.Left = 5
      Caption = 'Componentes do TManagerDataSet'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label38: TLabel
      Left = 34
      Top = 209
      Width = 129
      Height = 13
      Caption = 'ORMBr Manager ObjectSet'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label39: TLabel
      Left = 197
      Top = 209
      Width = 180
      Height = 13
      Margins.Left = 5
      Caption = 'Componentes do TManagerObjectSet'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label32: TLabel
      Left = 34
      Top = 61
      Width = 56
      Height = 13
      Caption = 'DBCBr Core'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label40: TLabel
      Left = 197
      Top = 63
      Width = 249
      Height = 13
      Margins.Left = 5
      Caption = 'Core do DBCBr Framework for Delphi (Depend'#234'ncia)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DBEBrConnectionMongoWire_dpk: TCheckBox
      Left = 12
      Top = 434
      Width = 16
      Height = 17
      Hint = 'DBEBrConnectionMongoWire.dpk'
      TabOrder = 17
    end
    object ORMBrLibrary_dpk: TCheckBox
      Left = 12
      Top = 26
      Width = 16
      Height = 17
      Hint = 'ORMBrLibrary.dpk'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 0
    end
    object ORMBrDriversLinks_dpk: TCheckBox
      Left = 12
      Top = 121
      Width = 16
      Height = 17
      Hint = 'ORMBrDriversLinks.dpk'
      TabOrder = 4
    end
    object DBEBrConnectionFireDAC_dpk: TCheckBox
      Left = 12
      Top = 254
      Width = 16
      Height = 17
      Hint = 'DBEBrConnectionFireDAC.dpk'
      TabOrder = 8
    end
    object DBEBrConnectionDBExpress_dpk: TCheckBox
      Left = 12
      Top = 275
      Width = 16
      Height = 17
      Hint = 'DBEBrConnectionDBExpress.dpk'
      TabOrder = 9
    end
    object DBEBrConnectionZeos_dpk: TCheckBox
      Left = 12
      Top = 296
      Width = 16
      Height = 17
      Hint = 'DBEBrConnectionZeos.dpk'
      TabOrder = 10
    end
    object DBEBrConnectionUniDAC_dpk: TCheckBox
      Left = 12
      Top = 316
      Width = 16
      Height = 17
      Hint = 'DBEBrConnectionUniDAC.dpk'
      TabOrder = 11
    end
    object DBEBrConnectionFIBPlus_dpk: TCheckBox
      Left = 12
      Top = 336
      Width = 16
      Height = 17
      Hint = 'DBEBrConnectionFIBPlus.dpk'
      TabOrder = 12
    end
    object DBEBrConnectionSQLDirect_dpk: TCheckBox
      Left = 12
      Top = 356
      Width = 16
      Height = 17
      Hint = 'DBEBrConnectionSQLDirect.dpk'
      TabOrder = 13
    end
    object DBEBrConnectionIBObjects_dpk: TCheckBox
      Left = 12
      Top = 376
      Width = 16
      Height = 17
      Hint = 'DBEBrConnectionIBObjects.dpk'
      TabOrder = 14
    end
    object DBEBrConnectionNexusDB_dpk: TCheckBox
      Left = 12
      Top = 396
      Width = 16
      Height = 17
      Hint = 'DBEBrConnectionNexusDB.dpk'
      TabOrder = 15
    end
    object DBEBrConnectionADO_dpk: TCheckBox
      Left = 12
      Top = 416
      Width = 16
      Height = 17
      Hint = 'DBEBrConnectionADO.dpk'
      TabOrder = 16
    end
    object ORMBrCore_dpk: TCheckBox
      Left = 12
      Top = 77
      Width = 16
      Height = 17
      Hint = 'ORMBrCore.dpk'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 3
    end
    object DBEBrCore_dpk: TCheckBox
      Left = 12
      Top = 43
      Width = 16
      Height = 17
      Hint = 'DBEBrCore.dpk'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 1
    end
    object ORMBrManagerClientDataSet_dpk: TCheckBox
      Left = 12
      Top = 169
      Width = 16
      Height = 17
      Hint = 'ORMBrManagerClientDataSet.dpk'
      TabOrder = 5
    end
    object ORMBrManagerFDMemTable_dpk: TCheckBox
      Left = 12
      Top = 188
      Width = 16
      Height = 17
      Hint = 'ORMBrManagerFDMemTable.dpk'
      TabOrder = 6
    end
    object ORMBrManagerObjectSet_dpk: TCheckBox
      Left = 12
      Top = 207
      Width = 16
      Height = 17
      Hint = 'ORMBrManagerObjectSet.dpk'
      TabOrder = 7
    end
    object DBCBrCore_dpk: TCheckBox
      Left = 12
      Top = 60
      Width = 16
      Height = 17
      Hint = 'DBCBrCore.dpk'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 2
    end
  end
end

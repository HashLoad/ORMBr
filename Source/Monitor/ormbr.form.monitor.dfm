object CommandMonitor: TCommandMonitor
  Left = 0
  Top = 0
  Caption = 'ORMBr - Monitor'
  ClientHeight = 346
  ClientWidth = 477
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    477
    346)
  PixelsPerInch = 96
  TextHeight = 13
  object MemoSQL: TMemo
    Left = 0
    Top = 0
    Width = 477
    Height = 311
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button1: TButton
    Left = 394
    Top = 315
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Limpar'
    TabOrder = 1
    OnClick = Button1Click
  end
end

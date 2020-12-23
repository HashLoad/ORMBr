{
                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Versão 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos é permitido copiar e distribuir cópias deste documento de
       licença, mas mudá-lo não é permitido.

       Esta versão da GNU Lesser General Public License incorpora
       os termos e condições da versão 3 da GNU General Public License
       Licença, complementado pelas permissões adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(ORMBr Livebindings)
  @created(29 Nov 2020)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Telegram : @IsaquePinheiro)
}

unit ormbr.fmx.controls;

interface

uses
  System.Generics.Collections,
  System.Rtti,
  System.TypInfo,
  System.Classes,
  System.SysUtils,
  System.Bindings.Expression,
  System.Bindings.Helper,
  //
  FMX.Controls,
  FMX.Edit,
  FMX.StdCtrls,
  FMX.ComboEdit,
  FMX.SpinBox,
  FMX.NumberBox,
  FMX.DateTimeCtrls,
  // ORMBr
  ormbr.controls.helpers;

type
  /// <summary>
  ///   TEdit Component
  /// </summary>
  TEdit = class(FMX.Edit.TEdit)
  protected
    procedure SetName(const NewName: TComponentName); override;
  public
    procedure DoChangeInternal(Sender: TObject);
  end;

  /// <summary>
  ///   TLabel Component
  /// </summary>
  TLabel = class(FMX.StdCtrls.TLabel)
  protected
    procedure SetName(const NewName: TComponentName); override;
  end;

  /// <summary>
  ///   TComboEdit Component
  /// </summary>
  TComboEdit = class(FMX.ComboEdit.TComboEdit)
  protected
    procedure SetName(const NewName: TComponentName); override;
  public
    procedure DoChangeInternal(Sender: TObject);
  end;

  /// <summary>
  ///   TProgressBar Component
  /// </summary>
  TProgressBar = class(FMX.StdCtrls.TProgressBar)
  protected
    procedure SetName(const NewName: TComponentName); override;
  end;

  /// <summary>
  ///   TSpinBox Component
  /// </summary>
  TSpinBox = class(FMX.SpinBox.TSpinBox)
  protected
    procedure SetName(const NewName: TComponentName); override;
  public
    procedure DoChangeInternal(Sender: TObject);
  end;

  /// <summary>
  ///   TNumberBox Component
  /// </summary>
  TNumberBox = class(FMX.NumberBox.TNumberBox)
  protected
    procedure SetName(const NewName: TComponentName); override;
  public
    procedure DoChangeInternal(Sender: TObject);
  end;

  /// <summary>
  ///   TDateEdit Component
  /// </summary>
  TDateEdit = class(FMX.DateTimeCtrls.TDateEdit)
  protected
    procedure SetName(const NewName: TComponentName); override;
  public
    procedure DoChangeInternal(Sender: TObject);
  end;

  /// <summary>
  ///   TTimeEdit Component
  /// </summary>
  TTimeEdit = class(FMX.DateTimeCtrls.TTimeEdit)
  protected
    procedure SetName(const NewName: TComponentName); override;
  public
    procedure DoChangeInternal(Sender: TObject);
  end;
  
implementation

{ TEdit }

procedure TEdit.DoChangeInternal(Sender: TObject);
begin
  if TListControls.ListFieldNames.ContainsKey(Self.Name) then
    TBindings.Notify(Self, TListControls.ListFieldNames.Items[Self.Name]);
end;

procedure TEdit.SetName(const NewName: TComponentName);
begin
  inherited SetName(NeWname);
  TListControls.ListComponents.AddOrSetValue(NewName, Self);
  Self.OnChange := DoChangeInternal;
end;

{ TLabel }

procedure TLabel.SetName(const NewName: TComponentName);
begin
  inherited SetName(NeWname);
  TListControls.ListComponents.AddOrSetValue(NewName, Self);
end;

{ TComboEdit }

procedure TComboEdit.DoChangeInternal(Sender: TObject);
begin
  if TListControls.ListFieldNames.ContainsKey(Self.Name) then
    TBindings.Notify(Self, TListControls.ListFieldNames.Items[Self.Name]);
end;

procedure TComboEdit.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  TListControls.ListComponents.AddOrSetValue(NewName, Self);
  Self.OnChange := DoChangeInternal;
end;

{ TProgressBar }

procedure TProgressBar.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  TListControls.ListComponents.AddOrSetValue(NewName, Self);
end;

{ TSpinBox }

procedure TSpinBox.DoChangeInternal(Sender: TObject);
begin
  if TListControls.ListFieldNames.ContainsKey(Self.Name) then
    TBindings.Notify(Self, TListControls.ListFieldNames.Items[Self.Name]);
end;

procedure TSpinBox.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  TListControls.ListComponents.AddOrSetValue(NewName, Self);
  Self.OnChange := DoChangeInternal;
end;

{ TNumberBox }

procedure TNumberBox.DoChangeInternal(Sender: TObject);
begin
  if TListControls.ListFieldNames.ContainsKey(Self.Name) then
    TBindings.Notify(Self, TListControls.ListFieldNames.Items[Self.Name]);
end;

procedure TNumberBox.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  TListControls.ListComponents.AddOrSetValue(NewName, Self);
  Self.OnChange := DoChangeInternal;
end;

{ TDateEdit }

procedure TDateEdit.DoChangeInternal(Sender: TObject);
begin
  if TListControls.ListFieldNames.ContainsKey(Self.Name) then
    TBindings.Notify(Self, TListControls.ListFieldNames.Items[Self.Name]);
end;

procedure TDateEdit.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  TListControls.ListComponents.AddOrSetValue(NewName, Self);
  Self.OnChange := DoChangeInternal;
end;

{ TTimeEdit }

procedure TTimeEdit.DoChangeInternal(Sender: TObject);
begin
  if TListControls.ListFieldNames.ContainsKey(Self.Name) then
    TBindings.Notify(Self, TListControls.ListFieldNames.Items[Self.Name]);
end;

procedure TTimeEdit.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  TListControls.ListComponents.AddOrSetValue(NewName, Self);
  Self.OnChange := DoChangeInternal;
end;

end.

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

unit ormbr.vcl.controls;

interface

uses
  System.Generics.Collections,
  System.Rtti,
  System.Classes,
  System.SysUtils,
  System.Bindings.Expression,
  System.Bindings.Helper,
  //
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Mask,
  Vcl.ExtCtrls,
  //
  Winapi.Windows,
  //
  ormbr.controls.helpers;

type
  /// <summary>
  ///   TEdit Component
  /// </summary>
  TEdit = class(Vcl.StdCtrls.TEdit)
  protected
    procedure Change; override;
    procedure SetName(const NewName: TComponentName); override;
  end;

  /// <summary>
  ///   TMaskEdit Component
  /// </summary>
  TMaskEdit = class(Vcl.Mask.TMaskEdit)
  protected
    procedure Change; override;
    procedure SetName(const NewName: TComponentName); override;
  end;

  /// <summary>
  ///   TLabel Component
  /// </summary>
  TLabel = class(Vcl.StdCtrls.TLabel)
  protected
    procedure SetName(const NewName: TComponentName); override;
  end;

  /// <summary>
  ///   TComboBox Component
  /// </summary>
  TComboBox = class(Vcl.StdCtrls.TComboBox)
  protected
    procedure Change; override;
    procedure SetName(const NewName: TComponentName); override;
  end;

  /// <summary>
  ///   TMemo Component
  /// </summary>
  TMemo = class(Vcl.StdCtrls.TMemo)
  protected
    procedure Change; override;
    procedure SetName(const NewName: TComponentName); override;
  end;

  /// <summary>
  ///   TProgressBar Component
  /// </summary>
  TProgressBar = class(Vcl.ComCtrls.TProgressBar)
  protected
    procedure SetName(const NewName: TComponentName); override;
  end;

  /// <summary>
  ///   TButtonedEdit Component
  /// </summary>
  TButtonedEdit = class(Vcl.ExtCtrls.TButtonedEdit)
  protected
    procedure Change; override;
    procedure SetName(const NewName: TComponentName); override;
  end;

implementation

{ TEdit }

procedure TEdit.Change;
begin
  inherited Change;
  if TListControls.ListFieldNames.ContainsKey(Self.Name) then
    TBindings.Notify(Self, TListControls.ListFieldNames.Items[Self.Name]);
end;

procedure TEdit.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  TListControls.ListComponents.AddOrSetValue(NewName, Self);
end;

{ TLabel }

procedure TLabel.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  TListControls.ListComponents.AddOrSetValue(NewName, Self);
end;

{ TMaskEdit }

procedure TMaskEdit.Change;
begin
  inherited Change;
  if TListControls.ListFieldNames.ContainsKey(Self.Name) then
    TBindings.Notify(Self, TListControls.ListFieldNames.Items[Self.Name]);
end;

procedure TMaskEdit.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  TListControls.ListComponents.AddOrSetValue(NewName, Self);
end;

{ TComboBox }

procedure TComboBox.Change;
begin
  inherited Change;
  if TListControls.ListFieldNames.ContainsKey(Self.Name) then
    TBindings.Notify(Self, TListControls.ListFieldNames.Items[Self.Name]);
end;

procedure TComboBox.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  TListControls.ListComponents.AddOrSetValue(NewName, Self);
end;

{ TProgressBar }

procedure TProgressBar.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  TListControls.ListComponents.AddOrSetValue(NewName, Self);
end;

{ TMemo }

procedure TMemo.Change;
begin
  inherited Change;
  if TListControls.ListFieldNames.ContainsKey(Self.Name) then
    TBindings.Notify(Self, TListControls.ListFieldNames.Items[Self.Name]);
end;

procedure TMemo.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  TListControls.ListComponents.AddOrSetValue(NewName, Self);
end;

{ TButtonedEdit }

procedure TButtonedEdit.Change;
begin
  inherited Change;
  if TListControls.ListFieldNames.ContainsKey(Self.Name) then
    TBindings.Notify(Self, TListControls.ListFieldNames.Items[Self.Name]);
end;

procedure TButtonedEdit.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  TListControls.ListComponents.AddOrSetValue(NewName, Self);
end;

end.

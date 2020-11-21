{
      ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi

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

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

{$INCLUDE ..\ormbr.inc}

unit ormbr.rtti.helper;

interface

uses
  DB,
  Rtti,
  Variants,
  Classes,
  SysUtils,
  StrUtils,
  TypInfo,
  dbcbr.rtti.helper,
  ormbr.types.nullable;

type
  TRttiPropertyHelper_ = class helper (TRttiPropertyHelper) for TRttiProperty
  public
    procedure SetNullableValue(AInstance: Pointer; ATypeInfo:
      PTypeInfo; AValue: Variant);
  end;

implementation

uses
  ormbr.utils;

{ TRttiPropertyHelper_ }

procedure TRttiPropertyHelper_.SetNullableValue(AInstance: Pointer;
  ATypeInfo: PTypeInfo; AValue: Variant);
var
  LUtils: IUtilSingleton;
begin
  LUtils := TUtilSingleton.GetInstance;
  if ATypeInfo = TypeInfo(Nullable<Integer>) then
    if TVarData(AValue).VType <= varNull then
      Self.SetValue(AInstance, TValue.From(Nullable<Integer>.Create(AValue)))
    else
      Self.SetValue(AInstance, TValue.From(Nullable<Integer>.Create(Integer(AValue))))
  else
  if ATypeInfo = TypeInfo(Nullable<String>) then
    Self.SetValue(AInstance, TValue.From(Nullable<String>.Create(AValue)))
  else
  if ATypeInfo = TypeInfo(Nullable<TDateTime>) then
    if TVarData(AValue).VType <= varNull then
      Self.SetValue(AInstance, TValue.From(Nullable<TDateTime>.Create(AValue)))
    else
      Self.SetValue(AInstance, TValue.From(Nullable<TDateTime>.Create(LUtils.Iso8601ToDateTime(AValue))))
  else
  if ATypeInfo = TypeInfo(Nullable<Currency>) then
    if TVarData(AValue).VType <= varNull then
      Self.SetValue(AInstance, TValue.From(Nullable<Currency>.Create(AValue)))
    else
      Self.SetValue(AInstance, TValue.From(Nullable<Currency>.Create(Currency(AValue))))
  else
  if ATypeInfo = TypeInfo(Nullable<Double>) then
    if TVarData(AValue).VType <= varNull then
      Self.SetValue(AInstance, TValue.From(Nullable<Double>.Create(AValue)))
    else
      Self.SetValue(AInstance, TValue.From(Nullable<Double>.Create(Currency(AValue))))
  else
  if ATypeInfo = TypeInfo(Nullable<Boolean>) then
    Self.SetValue(AInstance, TValue.From(Nullable<Boolean>.Create(AValue)))
  else
  if ATypeInfo = TypeInfo(Nullable<TDate>) then
    if TVarData(AValue).VType <= varNull then
      Self.SetValue(AInstance, TValue.From(Nullable<TDate>.Create(AValue)))
    else
      Self.SetValue(AInstance, TValue.From(Nullable<TDate>.Create(TDate(AValue))))
  else
  if ATypeInfo = TypeInfo(Nullable<TTime>) then
    if TVarData(AValue).VType <= varNull then
      Self.SetValue(AInstance, TValue.From(Nullable<TTime>.Create(AValue)))
    else
      Self.SetValue(AInstance, TValue.From(Nullable<TTime>.Create(TTime(AValue))));
end;

end.

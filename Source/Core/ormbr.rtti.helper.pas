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
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)
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
  jsonbr.utils,
  dbcbr.rtti.helper,
  ormbr.types.nullable;

type
  TRttiPropertyHelper_ = class helper (TRttiPropertyHelper) for TRttiProperty
  public
//    procedure SetNullableValue(AInstance: Pointer; ATypeInfo:
//      PTypeInfo; AValue: Variant);
    function GetValueNullable(const AInstance: Pointer; const ATypeInfo:
      PTypeInfo): TValue;
    procedure SetValueNullable(const AInstance: Pointer;
      const ATypeInfo: PTypeInfo; const AValue: Variant;
      const AUseISO8601DateFormat: Boolean = True); overload;
  end;

implementation

uses
  ormbr.utils;

{ TRttiPropertyHelper_ }

function TRttiPropertyHelper_.GetValueNullable(const AInstance: Pointer;
  const ATypeInfo: PTypeInfo): TValue;
begin
  if ATypeInfo = TypeInfo(Nullable<Integer>) then
    Result := TValue.From(Self.GetValue(AInstance).AsType<Nullable<Integer>>.ToVariant)
  else
  if ATypeInfo = TypeInfo(Nullable<Int64>) then
    Result := TValue.From(Self.GetValue(AInstance).AsType<Nullable<Int64>>.ToVariant)
  else
  if ATypeInfo = TypeInfo(Nullable<String>) then
    Result := TValue.From(Self.GetValue(AInstance).AsType<Nullable<String>>.ToVariant)
  else
  if ATypeInfo = TypeInfo(Nullable<TDateTime>) then
    Result := TValue.From(Self.GetValue(AInstance).AsType<Nullable<TDateTime>>.ToVariant)
  else
  if ATypeInfo = TypeInfo(Nullable<TDate>) then
    Result := TValue.From(Self.GetValue(AInstance).AsType<Nullable<TDate>>.ToVariant)
  else
  if ATypeInfo = TypeInfo(Nullable<TTime>) then
    Result := TValue.From(Self.GetValue(AInstance).AsType<Nullable<TTime>>.ToVariant)
  else
  if ATypeInfo = TypeInfo(Nullable<Currency>) then
    Result := TValue.From(Self.GetValue(AInstance).AsType<Nullable<Currency>>.ToVariant)
  else
  if ATypeInfo = TypeInfo(Nullable<Double>) then
    Result := TValue.From(Self.GetValue(AInstance).AsType<Nullable<Double>>.ToVariant)
  else
  if ATypeInfo = TypeInfo(Nullable<Boolean>) then
    Result := TValue.From(Self.GetValue(AInstance).AsType<Nullable<Boolean>>.ToVariant)
end;

//procedure TRttiPropertyHelper_.SetNullableValue(AInstance: Pointer;
//  ATypeInfo: PTypeInfo; AValue: Variant);
//begin
//  SetValueNullable(AInstance, ATypeInfo, AValue);
//end;

procedure TRttiPropertyHelper_.SetValueNullable(const AInstance: Pointer;
  const ATypeInfo: PTypeInfo; const AValue: Variant;
  const AUseISO8601DateFormat: Boolean);
begin
  if ATypeInfo = TypeInfo(Nullable<Integer>) then
    if TVarData(AValue).VType <= varNull then
      Self.SetValue(AInstance, TValue.From(Nullable<Integer>.Create(AValue)))
    else
      Self.SetValue(AInstance, TValue.From(Nullable<Integer>.Create(Integer(AValue))))
  else
  if ATypeInfo = TypeInfo(Nullable<Int64>) then
    if TVarData(AValue).VType <= varNull then
      Self.SetValue(AInstance, TValue.From(Nullable<Int64>.Create(AValue)))
    else
      Self.SetValue(AInstance, TValue.From(Nullable<Int64>.Create(Int64(AValue))))
  else
  if ATypeInfo = TypeInfo(Nullable<String>) then
    Self.SetValue(AInstance, TValue.From(Nullable<String>
                                   .Create(AValue)))
  else
  if ATypeInfo = TypeInfo(Nullable<Currency>) then
    if TVarData(AValue).VType <= varNull then
      Self.SetValue(AInstance, TValue.From(Nullable<Currency>
                                     .Create(AValue)))
    else
      Self.SetValue(AInstance, TValue.From(Nullable<Currency>
                                     .Create(Currency(AValue))))
  else
  if ATypeInfo = TypeInfo(Nullable<Double>) then
    if TVarData(AValue).VType <= varNull then
      Self.SetValue(AInstance, TValue.From(Nullable<Double>
                                     .Create(AValue)))
    else
      Self.SetValue(AInstance, TValue.From(Nullable<Double>
                                     .Create(Double(AValue))))
  else
  if ATypeInfo = TypeInfo(Nullable<Boolean>) then
    Self.SetValue(AInstance, TValue.From(Nullable<Boolean>
                                   .Create(AValue)))
  else
  if ATypeInfo = TypeInfo(Nullable<TDateTime>) then
    if TVarData(AValue).VType <= varNull then
      Self.SetValue(AInstance, TValue.From(Nullable<TDateTime>
                                     .Create(AValue)))
    else
      Self.SetValue(AInstance, TValue.From(Nullable<TDateTime>
                                     .Create(Iso8601ToDateTime(AValue, AUseISO8601DateFormat))))
  else
  if ATypeInfo = TypeInfo(Nullable<TDate>) then
    if TVarData(AValue).VType <= varNull then
      Self.SetValue(AInstance, TValue.From(Nullable<TDate>
                                     .Create(AValue)))
    else
      Self.SetValue(AInstance, TValue.From(Nullable<TDate>
                                     .Create(Iso8601ToDateTime(AValue, AUseISO8601DateFormat))))
  else
  if ATypeInfo = TypeInfo(Nullable<TTime>) then
    if TVarData(AValue).VType <= varNull then
      Self.SetValue(AInstance, TValue.From(Nullable<TTime>
                                     .Create(AValue)))
    else
      Self.SetValue(AInstance, TValue.From(Nullable<TTime>
                                     .Create(Iso8601ToDateTime(AValue, AUseISO8601DateFormat))));
end;

end.

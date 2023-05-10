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

unit ormbr.objects.helper;

interface

uses
  DB,
  Rtti,
  Variants,
  SysUtils,
  TypInfo, {Delphi 2010}
  Generics.Collections,
  ormbr.core.consts,
  ormbr.rtti.helper,
  dbcbr.mapping.popular,
  dbcbr.mapping.explorer,
  dbcbr.mapping.classes,
  dbcbr.mapping.attributes;

type
  TORMBrObject = class
  public
    constructor Create; virtual;
  end;

  TObjectHelper = class helper for TObject
  public
    function GetTable: Table;
    function GetResource: Resource;
    function GetNotServerUse: NotServerUse;
    function GetSubResource: SubResource;
    function &GetType(out AType: TRttiType): Boolean;
    function GetSequence: Sequence;
    function MethodCall(const AMethodName: string;
      const AParameters: array of TValue): TValue;
    procedure SetDefaultValue;
  end;

implementation

{ TObjectHelper }

function TObjectHelper.GetNotServerUse: NotServerUse;
var
  LType: TRttiType;
  LAttribute: TCustomAttribute;
begin
  Result := nil;
  if &GetType(LType) then
  begin
    for LAttribute in LType.GetAttributes do // NotServerUse
    begin
      if LAttribute is NotServerUse then
        Exit(NotServerUse(LAttribute));
    end;
    Exit(nil);
  end;
end;

function TObjectHelper.GetResource: Resource;
var
  LType: TRttiType;
  LAttribute: TCustomAttribute;
begin
  Result := nil;
  if &GetType(LType) then
  begin
    for LAttribute in LType.GetAttributes do // Resource
    begin
      if LAttribute is Resource then
        Exit(Resource(LAttribute));
    end;
    Exit(nil);
  end;
end;

function TObjectHelper.GetSequence: Sequence;
var
  LType: TRttiType;
  LAttribute: TCustomAttribute;
begin
  Result := nil;
  if &GetType(LType) then
  begin
    for LAttribute in LType.GetAttributes do
    begin
      if LAttribute is Sequence then // Sequence
        Exit(Sequence(LAttribute));
    end;
  end;
end;

function TObjectHelper.GetSubResource: SubResource;
var
  LType: TRttiType;
  LAttribute: TCustomAttribute;
begin
  if &GetType(LType) then
  begin
    for LAttribute in LType.GetAttributes do // SubResource
    begin
      if LAttribute is SubResource then
        Exit(SubResource(LAttribute));
    end;
    Exit(nil);
  end
  else
    Exit(nil);
end;

function TObjectHelper.GetTable: Table;
var
  LType: TRttiType;
  LAttribute: TCustomAttribute;
begin
  if &GetType(LType) then
  begin
    for LAttribute in LType.GetAttributes do
    begin
      if (LAttribute is Table) or (LAttribute is View) then // Table/View
        Exit(Table(LAttribute));
    end;
    Exit(nil);
  end
  else
    Exit(nil);
end;

function TObjectHelper.&GetType(out AType: TRttiType): Boolean;
var
  LContext: TRttiContext;
begin
  Result := False;
  if Assigned(Self) then
  begin
    LContext := TRttiContext.Create;
    try
      AType  := LContext.GetType(Self.ClassType);
      Result := Assigned(AType);
    finally
      LContext.Free;
    end;
  end;
end;

function TObjectHelper.MethodCall(const AMethodName: string;
  const AParameters: array of TValue): TValue;
var
  LContext: TRttiContext;
  LRttiType: TRttiType;
  LMethod: TRttiMethod;
begin
  LContext := TRttiContext.Create;
  try
    LRttiType := LContext.GetType(Self.ClassType);
    LMethod   := LRttiType.GetMethod(AMethodName);
    if Assigned(LMethod) then
       Result := LMethod.Invoke(Self, AParameters)
    else
       raise Exception.CreateFmt('Cannot find method "%s" in the object', [AMethodName]);
  finally
    LContext.Free;
  end;
end;

procedure TObjectHelper.SetDefaultValue;
var
  LColumns: TColumnMappingList;
  LColumn: TColumnMapping;
  LProperty: TRttiProperty;
  LValue: Variant;
begin
  LColumns := TMappingExplorer.GetMappingColumn(Self.ClassType);
  if LColumns = nil then
    Exit;

  for LColumn in LColumns do
  begin
    if Length(LColumn.DefaultValue) = 0 then
      Continue;

    LProperty := LColumn.ColumnProperty;
    LValue := StringReplace(LColumn.DefaultValue, '''', '', [rfReplaceAll]);

    case LProperty.PropertyType.TypeKind of
      tkString, tkWString, tkUString, tkWChar, tkLString, tkChar:
        LProperty.SetValue(Self, TValue.FromVariant(LValue).AsString);
      tkInteger, tkSet, tkInt64:
        LProperty.SetValue(Self, StrToIntDef(LValue, 0));
      tkFloat:
        begin
          if LProperty.PropertyType.Handle = TypeInfo(TDateTime) then // TDateTime
            LProperty.SetValue(Self, TValue.FromVariant(Date).AsType<TDateTime>)
          else
          if LProperty.PropertyType.Handle = TypeInfo(TDate) then // TDate
            LProperty.SetValue(Self, TValue.FromVariant(Date).AsType<TDate>)
          else
          if LProperty.PropertyType.Handle = TypeInfo(TTime) then// TTime
            LProperty.SetValue(Self, TValue.FromVariant(Time).AsType<TTime>)
          else
            LProperty.SetValue(Self, StrToFloatDef(LValue, 0));
        end;
      tkRecord:
        LProperty.SetValueNullable(Self, LProperty.PropertyType.Handle, LValue);
      tkEnumeration:
        begin
          case LColumn.FieldType of
            ftString, ftFixedChar:
              LProperty.SetValue(Self, LProperty.GetEnumStringValue(Self, LValue));
            ftInteger:
              LProperty.SetValue(Self, LProperty.GetEnumIntegerValue(Self, LValue));
            ftBoolean:
              LProperty.SetValue(Self, TValue.FromVariant(LValue).AsBoolean);
          else
            raise Exception.Create(cENUMERATIONSTYPEERROR);
          end;
        end;
    end;
  end;
end;

{ TORMBrObject }

constructor TORMBrObject.Create;
begin
  Self.SetDefaultValue;
end;

end.

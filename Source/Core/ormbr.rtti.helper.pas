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
  ormbr.mapping.rttiutils,
  ormbr.types.nullable,
  ormbr.mapping.attributes,
  ormbr.types.mapping,
  ormbr.mapping.classes;

type
  TRttiTypeHelper = class helper for TRttiType
  public
    function GetPrimaryKey: TArray<TCustomAttribute>;
    function GetAggregateField: TArray<TCustomAttribute>;
    function IsList: Boolean;
  end;

  TRttiPropertyHelper = class helper for TRttiProperty
  private
    function ResolveNullableValue(AObject: TObject): Boolean;
  public
    function  IsNoUpdate: Boolean;
    function  IsNoInsert: Boolean;
    function  IsNotNull: Boolean;
    function  IsNotCascade: Boolean;
    function  IsJoinColumn: Boolean;
    function  IsCheck: Boolean;
    function  IsUnique: Boolean;
    function  IsHidden: Boolean;
    function  IsNoValidate: Boolean;
    function  IsVirtualData: Boolean;
    function  IsBlob: Boolean;
    function  IsLazy: Boolean;
    function  IsNullable: Boolean;
    function  IsNullValue(AObject: TObject): Boolean;
    function  IsPrimaryKey(AClass: TClass): Boolean;
    function  IsList: Boolean;
    function  GetAssociation: TArray<Association>;
    function  GetRestriction: TCustomAttribute;
    function  GetDictionary: Dictionary;
    function  GetCalcField: TCustomAttribute;
    function  GetColumn: Column;
    function  GetNotNullConstraint: TCustomAttribute;
    function  GetMinimumValueConstraint: MinimumValueConstraint;
    function  GetMaximumValueConstraint: MaximumValueConstraint;
    function  GetNullableValue(AInstance: Pointer): TValue;
    function  GetTypeValue(ARttiType: TRttiType): TRttiType;
    function  GetObjectTheList: TObject;
    function  GetIndex: Integer;
    function  GetCascadeActions: TCascadeActions;
    function  GetEnumIntegerValue(const AInstance: TObject;
      AValue: Variant): TValue;
    function  GetEnumStringValue(const AInstance: TObject;
      AValue: Variant): TValue;
    function  GetEnumToFieldValue(const AInstance: TObject;
      AFieldType: TFieldType): TValue;
    procedure SetNullableValue(AInstance: Pointer; ATypeInfo:
      PTypeInfo; AValue: Variant);
  end;

implementation

uses
  ormbr.core.consts,
  ormbr.mapping.explorer,
  ormbr.types.blob,
  ormbr.utils,
  ormbr.objects.helper;

{ TRttiPropertyHelper }

function TRttiPropertyHelper.GetAssociation: TArray<Association>;
var
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  for LAttrib in Self.GetAttributes do
  begin
    if LAttrib is Association then // Association
    begin
      SetLength(Result, 1);
      Result[0] := Association(LAttrib);
    end;
  end;
end;

function TRttiPropertyHelper.GetCalcField: TCustomAttribute;
var
  LAttribute: TCustomAttribute;
begin
   for LAttribute in Self.GetAttributes do
   begin
      if LAttribute is CalcField then // CalcField
         Exit(LAttribute);
   end;
   Exit(nil);
end;

function TRttiPropertyHelper.GetCascadeActions: TCascadeActions;
var
  LAttribute: TCustomAttribute;
begin
   for LAttribute in Self.GetAttributes do
   begin
      if LAttribute is CascadeActions then // CascadeActions
         Exit(CascadeActions(LAttribute).CascadeActions);
   end;
   Exit([CascadeNone]);
end;

function TRttiPropertyHelper.GetColumn: Column;
var
  LAttribute: TCustomAttribute;
begin
   for LAttribute in Self.GetAttributes do
   begin
      if LAttribute is Column then // Column
         Exit(Column(LAttribute));
   end;
   Exit(nil);
end;

function TRttiPropertyHelper.GetDictionary: Dictionary;
var
  LAttribute: TCustomAttribute;
begin
   for LAttribute in Self.GetAttributes do
   begin
      if LAttribute is Dictionary then // Dictionary
         Exit(Dictionary(LAttribute));
   end;
   Exit(nil);
end;

function TRttiPropertyHelper.GetEnumIntegerValue(const AInstance: TObject;
  AValue: Variant): TValue;
var
  LEnumeration: TEnumerationMapping;
  LEnumerationList: TEnumerationMappingList;
  LIndex: Integer;
begin
  Result := nil;
  if AValue = Null then
    Exit;
  LEnumerationList := TMappingExplorer
                        .GetInstance.GetMappingEnumeration(AInstance.ClassType);
  if LEnumerationList = nil then
    Exit;
  for LEnumeration in LEnumerationList do
  begin
    if Self.PropertyType.AsOrdinal <> LEnumeration.OrdinalType then
      Continue;
    LIndex := LEnumeration.EnumValues.IndexOf(AValue);
    if LIndex > -1 then
      Result := TValue.FromOrdinal(Self.PropertyType.Handle, LIndex);
  end;
end;

function TRttiPropertyHelper.GetEnumStringValue(const AInstance: TObject;
  AValue: Variant): TValue;
var
  LEnumeration: TEnumerationMapping;
  LEnumerationList: TEnumerationMappingList;
  LIndex: Integer;
begin
  Result := nil;
  LEnumerationList := TMappingExplorer
                        .GetInstance.GetMappingEnumeration(AInstance.ClassType);
  if LEnumerationList = nil then
    Exit;
  for LEnumeration in LEnumerationList do
  begin
    if Self.PropertyType.AsOrdinal <> LEnumeration.OrdinalType then
      Continue;
    LIndex := LEnumeration.EnumValues.IndexOf(AValue);
    if LIndex > -1 then
      Result := TValue.FromOrdinal(Self.PropertyType.Handle, LIndex);
  end;
end;

function TRttiPropertyHelper.GetEnumToFieldValue(const AInstance: TObject;
  AFieldType: TFieldType): TValue;
var
  LEnumeration: TEnumerationMapping;
  LEnumerationList: TEnumerationMappingList;
  LValue: TValue;
begin
  Result := nil;
  LEnumerationList := TMappingExplorer
                        .GetInstance.GetMappingEnumeration(AInstance.ClassType);
  if LEnumerationList <> nil then
  begin
    LValue := Self.GetValue(AInstance);
    if LValue.AsOrdinal < 0 then
      Exit;
    for LEnumeration in LEnumerationList do
    begin
      if Self.PropertyType.AsOrdinal <> LEnumeration.OrdinalType then
        Continue;
      case AFieldType of
        ftFixedChar:
          Result := TValue.From<Char>(LEnumeration.EnumValues[LValue.AsOrdinal][1]);
        ftString:
          Result := TValue.From<string>(LEnumeration.EnumValues[LValue.AsOrdinal]);
        ftInteger:
          Result := TValue.From<Integer>(StrToIntDef(LEnumeration.EnumValues[LValue.AsOrdinal], 0));
        ftBoolean:
          Result := TValue.From<Boolean>(StrToBoolDef(LEnumeration.EnumValues[LValue.AsOrdinal], Boolean(0)));
      else
        raise Exception.Create(cENUMERATIONSTYPEERROR);
      end
    end;
  end;
  /// <summary>
  ///   Usar tipo Boolean nativo na propriedade da classe modelo.
  /// </summary>
  if Result.IsEmpty then
  begin
    case AFieldType of
      ftBoolean:
        Result := TValue.From<Variant>(Self.GetValue(AInstance).AsVariant);
    else
      raise Exception.Create(cENUMERATIONSTYPEERROR);
    end
  end;
end;

function TRttiPropertyHelper.GetIndex: Integer;
begin
  Result := (Self as TRttiInstanceProperty).Index +1;
end;

function TRttiPropertyHelper.GetTypeValue(ARttiType: TRttiType): TRttiType;
var
  LTypeName: string;
  LContext: TRttiContext;
begin
   LContext := TRttiContext.Create;
   try
     LTypeName := ARttiType.ToString;
     LTypeName := StringReplace(LTypeName,'TObjectList<','',[]);
     LTypeName := StringReplace(LTypeName,'TList<','',[]);
     LTypeName := StringReplace(LTypeName,'>','',[]);
     ///
     Result := LContext.FindType(LTypeName);
   finally
     LContext.Free;
   end;
end;

function TRttiPropertyHelper.GetNotNullConstraint: TCustomAttribute;
var
  LAttribute: TCustomAttribute;
begin
   for LAttribute in Self.GetAttributes do
   begin
      if LAttribute is NotNullConstraint then // NotNullConstraint
         Exit(LAttribute);
   end;
   Exit(nil);
end;

function TRttiPropertyHelper.GetNullableValue(AInstance: Pointer): TValue;
var
  LValue: TValue;
  LValueField: TRttiField;
  LHasValueField: TRttiField;
begin
  Result := nil;
  if not Assigned(AInstance) then
    Exit;
    
  if Self.IsNullable then
  begin
    LValue := Self.GetValue(AInstance);
    LHasValueField := Self.PropertyType.GetField('FHasValue');
    if not Assigned(LHasValueField) then
      Exit;

    LValueField := Self.PropertyType.GetField('FValue');
    if Assigned(LValueField) then
      Result := LValueField.GetValue(LValue.GetReferenceToRawData);
  end
  else
    Result := Self.GetValue(AInstance);
end;

function TRttiPropertyHelper.GetObjectTheList: TObject;
var
  LPropertyType: TRttiType;
  LObject: TObject;
begin
  LObject := nil;
  LPropertyType := Self.PropertyType;
  if IsList then
  begin
    LPropertyType := Self.GetTypeValue(LPropertyType);
    LObject := LPropertyType.AsInstance.MetaclassType.Create;
    LObject.MethodCall('Create', []);
  end;
  Result := LObject;
end;

function TRttiPropertyHelper.GetRestriction: TCustomAttribute;
var
  LAttribute: TCustomAttribute;
begin
   for LAttribute in Self.GetAttributes do
   begin
      if LAttribute is Restrictions then // Restrictions
         Exit(LAttribute);
   end;
   Exit(nil);
end;

function TRttiPropertyHelper.GetMaximumValueConstraint: MaximumValueConstraint;
var
  LAttribute: TCustomAttribute;
begin
   for LAttribute in Self.GetAttributes do
   begin
      if LAttribute is MaximumValueConstraint then // MaximumValueConstraint
         Exit(MaximumValueConstraint(LAttribute));
   end;
   Exit(nil);
end;

function TRttiPropertyHelper.GetMinimumValueConstraint: MinimumValueConstraint;
var
  LAttribute: TCustomAttribute;
begin
   for LAttribute in Self.GetAttributes do
   begin
      if LAttribute is MinimumValueConstraint then // MinimumValueConstraint
         Exit(MinimumValueConstraint(LAttribute));
   end;
   Exit(nil);
end;

function TRttiPropertyHelper.IsNotCascade: Boolean;
var
  LAttribute: Association;
  LAssociationList: TArray<Association>;
begin
  Result := False;
  LAssociationList := Self.GetAssociation;
  if LAssociationList = nil then
    Exit;
  LAttribute := LAssociationList[0];
  if LAttribute = nil then
    Exit;
  if CascadeNone in Self.GetCascadeActions then
    Result := True;
end;

function TRttiPropertyHelper.IsBlob: Boolean;
const
  LPrefixString = 'TBlob';
var
  LTypeInfo: PTypeInfo;
begin
  LTypeInfo := Self.PropertyType.Handle;
  Result := Assigned(LTypeInfo) and (LTypeInfo.Kind = tkRecord)
                                and StartsText(LPrefixString, GetTypeName(LTypeInfo));
end;

function TRttiPropertyHelper.IsCheck: Boolean;
var
  LAttribute: TCustomAttribute;
begin
   for LAttribute in Self.GetAttributes do
   begin
      if LAttribute is Check then // Check
         Exit(True);
   end;
   Exit(False);
end;

function TRttiPropertyHelper.IsVirtualData: Boolean;
var
  LAttribute: TCustomAttribute;
begin
   LAttribute := Self.GetRestriction;
   if LAttribute <> nil then
   begin
     if VirtualData in Restrictions(LAttribute).Restrictions then
       Exit(True);
   end;
   Exit(False);
end;

function TRttiPropertyHelper.IsHidden: Boolean;
var
  LAttribute: TCustomAttribute;
begin
   LAttribute := Self.GetRestriction;
   if LAttribute <> nil then
   begin
     if Hidden in Restrictions(LAttribute).Restrictions then
       Exit(True);
   end;
   Exit(False);
end;

function TRttiPropertyHelper.IsNoInsert: Boolean;
var
  LAttribute: TCustomAttribute;
begin
   LAttribute := Self.GetRestriction;
   if LAttribute <> nil then
   begin
     if NoInsert in Restrictions(LAttribute).Restrictions then
       Exit(True);
   end;
   Exit(False);
end;

function TRttiPropertyHelper.IsJoinColumn: Boolean;
var
  LAttribute: TCustomAttribute;
begin
   for LAttribute in Self.GetAttributes do
   begin
      if LAttribute is JoinColumn then // JoinColumn
         Exit(True);
   end;
   Exit(False);
end;

function TRttiPropertyHelper.IsLazy: Boolean;
const
  LPrefixString = 'Lazy';
var
  LTypeInfo: PTypeInfo;
begin
  LTypeInfo := Self.PropertyType.Handle;
  Result := Assigned(LTypeInfo) and (LTypeInfo.Kind = tkRecord)
                                and StartsText(LPrefixString, GetTypeName(LTypeInfo));
end;

function TRttiPropertyHelper.IsList: Boolean;
var
  LTypeName: string;
begin
  Result := False;
  LTypeName := Self.PropertyType.ToString;
  if Pos('TObjectList<', LTypeName) > 0 then
    Result := True
  else
  if Pos('TList<', LTypeName) > 0 then
    Result := True
end;

function TRttiPropertyHelper.IsNotNull: Boolean;
var
  LAttribute: TCustomAttribute;
begin
  Result := False;
  LAttribute := Self.GetRestriction;
  if LAttribute = nil then
    Exit;

  if NotNull in Restrictions(LAttribute).Restrictions then
    Exit(True);
end;

function TRttiPropertyHelper.IsNoUpdate: Boolean;
var
  LAttribute: TCustomAttribute;
begin
  Result := False;
  LAttribute := Self.GetRestriction;
  if LAttribute = nil then
    Exit;

  if NoUpdate in Restrictions(LAttribute).Restrictions then
    Exit(True);
end;

function TRttiPropertyHelper.IsNullable: Boolean;
const
  LPrefixString = 'Nullable<';
var
  LTypeInfo: PTypeInfo;
begin
  LTypeInfo := Self.PropertyType.Handle;
  Result := Assigned(LTypeInfo) and (LTypeInfo.Kind = tkRecord)
                                and StartsText(LPrefixString, GetTypeName(LTypeInfo));
end;

function TRttiPropertyHelper.ResolveNullableValue(AObject: TObject): Boolean;
var
  LValue: TValue;
begin
  Result := False;
  LValue := GetNullableValue(AObject);
  if LValue.AsVariant = Null then
    Exit(True);

  if LVAlue.Kind in [tkString, tkUString, tkLString, tkWString
                    {$IFDEF DELPHI22_UP}
                    , tkAnsiChar, tkWideChar, tkAnsiString, tkWideString
                    , tkShortString, tkUnicodeString
                    {$ENDIF}] then

  begin
    if LValue.AsType<String> = '' then
      Exit(True);
  end
  else
  if LValue.Kind in [tkInteger, tkInt64] then
  begin
    if LValue.AsType<Integer> = 0 then
       Exit(True);
  end
  else
  if LValue.Kind in [tkFloat] then
  begin
    if LValue.TypeInfo = TypeInfo(TDateTime) then
    begin
      if LValue.AsType<TDateTime> = 0 then
        Exit(True);
    end
    else
    if LValue.TypeInfo = TypeInfo(TDate) then
    begin
      if LValue.AsType<TDate> = 0 then
        Exit(True);
    end
    else
    if LValue.TypeInfo = TypeInfo(TTime) then
    begin
      if LValue.AsType<TTime> = 0 then
        Exit(True);
    end
    else
    begin
      if LValue.AsType<Double> = 0 then
        Exit(True);
    end;
  end;
end;

function TRttiPropertyHelper.IsNullValue(AObject: TObject): Boolean;
begin
  Result := False;
//  if (not Self.IsNotNull) and (Self.IsNullable) then
//    Exit(ResolveNullableValue(AObject));

  if Self.IsNotNull then
    Exit(False);

  if Self.IsNullable then
    Exit(ResolveNullableValue(AObject));
end;

function TRttiPropertyHelper.IsPrimaryKey(AClass: TClass): Boolean;
var
  LPrimaryKey: TPrimaryKeyMapping;
  LColumnName: string;
begin
  Result := False;
  LPrimaryKey := TMappingExplorer.GetInstance.GetMappingPrimaryKey(AClass);
  if LPrimaryKey = nil then
    Exit;

  for LColumnName in LPrimaryKey.Columns do
    if SameText(LColumnName, Column(Self.GetColumn).ColumnName) then
      Exit(True);
end;

function TRttiPropertyHelper.IsNoValidate: Boolean;
var
  LAttribute: TCustomAttribute;
begin
  Result := False;
  LAttribute := Self.GetRestriction;
  if LAttribute = nil then
    Exit;

  if NoValidate in Restrictions(LAttribute).Restrictions then
    Exit(True);
end;

function TRttiPropertyHelper.IsUnique: Boolean;
var
  LAttribute: TCustomAttribute;
begin
  Result := False;
  LAttribute := Self.GetRestriction;
  if LAttribute = nil then
    Exit;

  if Unique in Restrictions(LAttribute).Restrictions then
    Exit(True);
end;

procedure TRttiPropertyHelper.SetNullableValue(AInstance: Pointer;
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

{ TRttiTypeHelper }

function TRttiTypeHelper.GetAggregateField: TArray<TCustomAttribute>;
var
  LAttrib: TCustomAttribute;
  LLength: Integer;
begin
  Result := nil;
  LLength := -1;
  for LAttrib in Self.GetAttributes do
  begin
     if not (LAttrib is AggregateField) then // AggregateField
       Continue;

     Inc(LLength);
     SetLength(Result, LLength+1);
     Result[LLength] := LAttrib;
  end;
end;

function TRttiTypeHelper.GetPrimaryKey: TArray<TCustomAttribute>;
var
  LAttrib: TCustomAttribute;
  LLength: Integer;
begin
  Result := nil;
  LLength := -1;
  for LAttrib in Self.GetAttributes do
  begin
     if not (LAttrib is PrimaryKey) then // PrimaryKey
       Continue;

     Inc(LLength);
     SetLength(Result, LLength+1);
     Result[LLength] := LAttrib;
  end;
end;

function TRttiTypeHelper.IsList: Boolean;
begin
  if Pos('TObjectList<', Self.AsInstance.Name) > 0 then
    Result := True
  else
  if Pos('TList<', Self.AsInstance.Name) > 0 then
    Result := True
  else
    Result := False;
end;

end.

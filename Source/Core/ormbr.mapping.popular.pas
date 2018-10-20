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

unit ormbr.mapping.popular;

interface

uses
  DB,
  Classes,
  Rtti,
  TypInfo,
  StrUtils,
  Generics.Collections,
  ormbr.mapping.rttiutils,
  ormbr.mapping.attributes,
  ormbr.mapping.classes,
  ormbr.rtti.helper,
  ormbr.types.mapping,
  ormbr.mapping.explorerstrategy;

type
  TMappingPopular = class
  private
    FMappingExplorerStrategy: TMappingExplorerStrategy;
  public
    constructor Create(AMappingExplorerStrategy: TMappingExplorerStrategy);
    function PopularTable(ARttiType: TRttiType): TTableMapping;
    function PopularOrderBy(ARttiType: TRttiType): TOrderByMapping;
    function PopularSequence(ARttiType: TRttiType): TSequenceMapping;
    function PopularPrimaryKey(ARttiType: TRttiType): TPrimaryKeyMapping;
    function PopularForeignKey(ARttiType: TRttiType): TForeignKeyMappingList;
    function PopularIndexe(ARttiType: TRttiType): TIndexeMappingList;
    function PopularCheck(ARttiType: TRttiType): TCheckMappingList;
    function PopularColumn(ARttiType: TRttiType; AClass: TClass): TColumnMappingList;
    function PopularCalcField(ARttiType: TRttiType; AClass: TClass): TCalcFieldMappingList;
    function PopularAssociation(ARttiType: TRttiType): TAssociationMappingList;
    function PopularJoinColumn(ARttiType: TRttiType): TJoinColumnMappingList;
    function PopularTrigger(ARttiType: TRttiType): TTriggerMappingList;
    function PopularView(ARttiType: TRttiType): TViewMapping;
    function PopularFieldEvents(ARttiType: TRttiType): TFieldEventsMappingList;
    function PopularEnumeration(ARttiType: TRttiType): TEnumerationMappingList;
  end;

implementation

uses
  ormbr.objects.helper;

{ TMappingPopular }

constructor TMappingPopular.Create(AMappingExplorerStrategy: TMappingExplorerStrategy);
begin
  FMappingExplorerStrategy := AMappingExplorerStrategy;
end;

function TMappingPopular.PopularCalcField(ARttiType: TRttiType;
  AClass: TClass): TCalcFieldMappingList;
var
  LProperty: TRttiProperty;
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  for LProperty in ARttiType.GetProperties do
  begin
    for LAttrib in LProperty.GetAttributes do
    begin
      if LAttrib is CalcField then // CalcField
      begin
        if Result = nil then
          Result := TCalcFieldMappingList.Create;
        Result.Add(TCalcFieldMapping.Create);
        Result.Last.FieldName := CalcField(LAttrib).FieldName;
        Result.Last.FieldType := CalcField(LAttrib).FieldType;
        Result.Last.Size := CalcField(LAttrib).Size;
        Result.Last.PropertyRtti := LProperty;
      end;
    end;
  end;
end;

function TMappingPopular.PopularCheck(ARttiType: TRttiType): TCheckMappingList;
var
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  for LAttrib in ARttiType.GetAttributes do
  begin
    if LAttrib is Check then // Check
    begin
       if Result = nil then
          Result := TCheckMappingList.Create;
       Result.Add(TCheckMapping.Create(Check(LAttrib).Name,
                                       Check(LAttrib).Condition,
                                       Check(LAttrib).Description));
    end;
  end;
end;

function TMappingPopular.PopularColumn(ARttiType: TRttiType;
  AClass: TClass): TColumnMappingList;
var
  LProperty: TRttiProperty;
  LAttrib: TCustomAttribute;
  LDictionary: TCustomAttribute;
begin
  Result := nil;
  for LProperty in ARttiType.GetProperties do
  begin
    LDictionary := LProperty.GetDictionary;
    for LAttrib in LProperty.GetAttributes do
    begin
      if LAttrib is Column then // Column
      begin
        if Result = nil then
          Result := TColumnMappingList.Create;
        Result.Add(TColumnMapping.Create);
        Result.Last.ColumnName := Column(LAttrib).ColumnName;
        Result.Last.FieldType :=  Column(LAttrib).FieldType;
        Result.Last.Scale := Column(LAttrib).Scale;
        Result.Last.Size := Column(LAttrib).Size;
        Result.Last.Precision := Column(LAttrib).Precision;
        Result.Last.PropertyRtti := LProperty;
        Result.Last.FieldIndex := Result.Count -1;
        Result.Last.IsJoinColumn := LProperty.IsJoinColumn;
        Result.Last.IsNotNull := LProperty.IsNotNull;
        Result.Last.IsUnique := LProperty.IsUnique;
        Result.Last.IsNoUpdate := LProperty.IsNoUpdate;
        Result.Last.IsNoInsert := LProperty.IsNoInsert;
        Result.Last.IsCheck := LProperty.IsCheck;
        Result.Last.IsNoValidate := LProperty.IsNoValidate;
        Result.Last.IsHidden := LProperty.IsHidden;
        Result.Last.IsPrimaryKey := LProperty.IsPrimaryKey(AClass);
        Result.Last.DefaultValue := '';

        if LDictionary <> nil then
         if not MatchStr(Dictionary(LDictionary).DefaultExpression, ['Date', 'Now']) then
           Result.Last.DefaultValue := Dictionary(LDictionary).DefaultExpression
      end;
    end;
  end;
end;

function TMappingPopular.PopularEnumeration(ARttiType: TRttiType): TEnumerationMappingList;
var
  LProperty: TRttiProperty;
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  for LProperty in ARttiType.GetProperties do
  begin
    for LAttrib in LProperty.GetAttributes do
    begin
      if LAttrib is Enumeration then // Enumeration
      begin
         if Result = nil then
            Result := TEnumerationMappingList.Create;
        Result.Add(TEnumerationMapping.Create(LProperty.PropertyType.AsOrdinal,
                                              Enumeration(LAttrib).EnumType,
                                              Enumeration(LAttrib).EnumValues));
      end;
    end;
  end;
end;

function TMappingPopular.PopularFieldEvents(ARttiType: TRttiType): TFieldEventsMappingList;
var
  LProperty: TRttiProperty;
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  for LProperty in ARttiType.GetProperties do
  begin
    for LAttrib in LProperty.GetAttributes do
    begin
      if LAttrib is FieldEvents then // FieldEvents
      begin
         if Result = nil then
            Result := TFieldEventsMappingList.Create;
         Result.Add(TFieldEventsMapping.Create(Column(LProperty.GetColumn).ColumnName,
                                               FieldEvents(LAttrib).Events));
      end;
    end;
  end;
end;

function TMappingPopular.PopularForeignKey(ARttiType: TRttiType): TForeignKeyMappingList;
var
  LProperty: TRttiProperty;
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  /// Atributos da classe
  for LAttrib in ARttiType.GetAttributes do
  begin
    if LAttrib is ForeignKey then // ForeignKey
    begin
       if Result = nil then
          Result := TForeignKeyMappingList.Create;
       Result.Add(TForeignKeyMapping.Create(ForeignKey(LAttrib).Name,
                                            ForeignKey(LAttrib).TableNameRef,
                                            ForeignKey(LAttrib).FromColumns,
                                            ForeignKey(LAttrib).ToColumns,
                                            ForeignKey(LAttrib).RuleDelete,
                                            ForeignKey(LAttrib).RuleUpdate,
                                            ForeignKey(LAttrib).Description));
    end;
  end;
  /// Atributos das propriedades
  for LProperty in ARttiType.GetProperties do
  begin
    for LAttrib in LProperty.GetAttributes do
    begin
      if LAttrib is ForeignKey then // ForeignKey
      begin
         if Result = nil then
            Result := TForeignKeyMappingList.Create;
         Result.Add(TForeignKeyMapping.Create(ForeignKey(LAttrib).Name,
                                              ForeignKey(LAttrib).TableNameRef,
                                              ForeignKey(LAttrib).FromColumns,
                                              ForeignKey(LAttrib).ToColumns,
                                              ForeignKey(LAttrib).RuleDelete,
                                              ForeignKey(LAttrib).RuleUpdate,
                                              ForeignKey(LAttrib).Description));
      end;
    end;
  end;
end;

function TMappingPopular.PopularIndexe(ARttiType: TRttiType): TIndexeMappingList;
var
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  for LAttrib in ARttiType.GetAttributes do
  begin
    if LAttrib is Indexe then // Indexe
    begin
       if Result = nil then
          Result := TIndexeMappingList.Create;
       Result.Add(TIndexeMapping.Create(Indexe(LAttrib).Name,
                                        Indexe(LAttrib).Columns,
                                        Indexe(LAttrib).SortingOrder,
                                        Indexe(LAttrib).Unique,
                                        Indexe(LAttrib).Description));
    end;
  end;
end;

function TMappingPopular.PopularJoinColumn(ARttiType: TRttiType): TJoinColumnMappingList;
var
  LProperty: TRttiProperty;
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  for LProperty in ARttiType.GetProperties do
  begin
    for LAttrib in LProperty.GetAttributes do
    begin
      if LAttrib is JoinColumn then // JoinColumn
      begin
        if Length(JoinColumn(LAttrib).RefTableName) > 0 then
        begin
          if Result = nil then
             Result := TJoinColumnMappingList.Create;
          Result.Add(TJoinColumnMapping.Create(JoinColumn(LAttrib).ColumnName,
                                               JoinColumn(LAttrib).RefTableName,
                                               JoinColumn(LAttrib).RefColumnName,
                                               JoinColumn(LAttrib).RefColumnNameSelect,
                                               JoinColumn(LAttrib).Join,
                                               JoinColumn(LAttrib).AliasColumn,
                                               JoinColumn(LAttrib).AliasRefTable));
        end;
      end;
    end;
  end;
end;

function TMappingPopular.PopularOrderBy(ARttiType: TRttiType): TOrderByMapping;
var
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  for LAttrib in ARttiType.GetAttributes do
  begin
    if LAttrib is OrderBy then // OrderBy
    begin
      Result := TOrderByMapping.Create;
      Result.ColumnsName := OrderBy(LAttrib).ColumnsName;
    end;
  end;
end;

function TMappingPopular.PopularPrimaryKey(ARttiType: TRttiType): TPrimaryKeyMapping;
var
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  for LAttrib in ARttiType.GetAttributes do
  begin
    if LAttrib is PrimaryKey then // PrimaryKey
    begin
       Result := TPrimaryKeyMapping.Create(PrimaryKey(LAttrib).Columns,
                                           PrimaryKey(LAttrib).SequenceType = AutoInc,
                                           PrimaryKey(LAttrib).SortingOrder,
                                           PrimaryKey(LAttrib).Unique,
                                           PrimaryKey(LAttrib).Description);
    end;
  end;
end;

function TMappingPopular.PopularSequence(ARttiType: TRttiType): TSequenceMapping;
var
  LAttrib: TCustomAttribute;
  LTable: Table;
begin
  Result := nil;
  LTable := nil;
  for LAttrib in ARttiType.GetAttributes do
  begin
    if LAttrib is Table then // Table
      LTable := Table(LAttrib);

    if LAttrib is Sequence then // Sequence
    begin
      Result := TSequenceMapping.Create;
      Result.Name := Sequence(LAttrib).Name;
      Result.Initial := Sequence(LAttrib).Initial;
      Result.Increment := Sequence(LAttrib).Increment;
    end;
  end;
  if Result <> nil then
    Result.TableName := LTable.Name;
end;

function TMappingPopular.PopularAssociation(ARttiType: TRttiType): TAssociationMappingList;
var
  LRttiType: TRttiType;
  LProperty: TRttiProperty;
  LAssociation: TCustomAttribute;
  LColumns: TArray<string>;
begin
  Result := nil;
  for LProperty in ARttiType.GetProperties do
  begin
    for LAssociation in LProperty.GetAttributes do
    begin
      if LAssociation is Association then // Association
      begin
        SetLength(LColumns, 0);
        if Length(Association(LAssociation).ColumnsNameRef) > 0 then
        begin
          if Result = nil then
             Result := TAssociationMappingList.Create;

          LRttiType := ARttiType.GetProperty(LProperty.Name).PropertyType;
          LRttiType := LProperty.GetTypeValue(LRttiType);
          if LRttiType <> nil then
             Result.Add(TAssociationMapping.Create(Association(LAssociation).Multiplicity,
                                                   Association(LAssociation).ColumnsName,
                                                   Association(LAssociation).ColumnsNameRef,
                                                   LRttiType.AsInstance.MetaclassType.ClassName,
                                                   LProperty,
                                                   Association(LAssociation).Lazy,
                                                   LProperty.GetCascadeActions))
          else
             Result.Add(TAssociationMapping.Create(Association(LAssociation).Multiplicity,
                                                   Association(LAssociation).ColumnsName,
                                                   Association(LAssociation).ColumnsNameRef,
                                                   LProperty.PropertyType.Name,
                                                   LProperty,
                                                   Association(LAssociation).Lazy,
                                                   LProperty.GetCascadeActions));
        end;
      end;
    end;
  end;
end;

function TMappingPopular.PopularTable(ARttiType: TRttiType): TTableMapping;
var
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  for LAttrib in ARttiType.GetAttributes do
  begin
    if (LAttrib is Table) or (LAttrib is View) then // Table/View
    begin
      Result := TTableMapping.Create;
      Result.Name := Table(LAttrib).Name;
      Result.Description := Table(LAttrib).Description;
      Result.Schema := '';
    end;
  end;
end;

function TMappingPopular.PopularTrigger(ARttiType: TRttiType): TTriggerMappingList;
var
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  for LAttrib in ARttiType.GetAttributes do
  begin
    if LAttrib is Trigger then // Trigger
    begin
      if Result = nil then
         Result := TTriggerMappingList.Create;
      Result.Add(TTriggerMapping.Create(Trigger(LAttrib).Name,
                                        ''));
    end;
  end;
end;

function TMappingPopular.PopularView(ARttiType: TRttiType): TViewMapping;
var
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  for LAttrib in ARttiType.GetAttributes do
  begin
    if LAttrib is View then // View
    begin
      Result := TViewMapping.Create;
      Result.Name := View(LAttrib).Name;
      Result.Description := View(LAttrib).Description;
      Result.Script := '';
    end;
  end;
end;

end.


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
  SysUtils,
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
    function PopularCalcField(ARttiType: TRttiType): TCalcFieldMappingList;
    function PopularAssociation(ARttiType: TRttiType): TAssociationMappingList;
    function PopularJoinColumn(ARttiType: TRttiType): TJoinColumnMappingList;
    function PopularTrigger(ARttiType: TRttiType): TTriggerMappingList;
    function PopularView(ARttiType: TRttiType): TViewMapping;
    function PopularFieldEvents(ARttiType: TRttiType): TFieldEventsMappingList;
    function PopularEnumeration(ARttiType: TRttiType): TEnumerationMappingList;
    function PopularPrimaryKeyColumns(ARttiType: TRttiType;
      AClass: TClass): TPrimaryKeyColumnsMapping;
  end;

implementation

uses
  ormbr.objects.helper,
  ormbr.mapping.explorer;

{ TMappingPopular }

constructor TMappingPopular.Create(AMappingExplorerStrategy: TMappingExplorerStrategy);
begin
  FMappingExplorerStrategy := AMappingExplorerStrategy;
end;

function TMappingPopular.PopularCalcField(ARttiType: TRttiType): TCalcFieldMappingList;
var
  LProperty: TRttiProperty;
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  for LProperty in ARttiType.GetProperties do
  begin
    for LAttrib in LProperty.GetAttributes do
    begin
      if not (LAttrib is CalcField) then // CalcField
        Continue;

      if Result = nil then
        Result := TCalcFieldMappingList.Create;
      Result.Add(TCalcFieldMapping.Create);
      Result.Last.FieldName := CalcField(LAttrib).FieldName;
      Result.Last.FieldType := CalcField(LAttrib).FieldType;
      Result.Last.Size := CalcField(LAttrib).Size;
      Result.Last.CalcProperty := LProperty;
      Result.Last.CalcDictionary := LProperty.GetDictionary;
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
    if not (LAttrib is Check) then // Check
      Continue;

    if Result = nil then
       Result := TCheckMappingList.Create;
    Result.Add(TCheckMapping.Create(Check(LAttrib).Name,
                                    Check(LAttrib).Condition,
                                    Check(LAttrib).Description));
  end;
end;

function TMappingPopular.PopularColumn(ARttiType: TRttiType;
  AClass: TClass): TColumnMappingList;
var
  LProperty: TRttiProperty;
  LAttrib: TCustomAttribute;
  LColumn: Column;
begin
  Result := nil;
  for LProperty in ARttiType.GetProperties do
  begin
    for LAttrib in LProperty.GetAttributes do
    begin
      if not (LAttrib is Column) then // Column
        Continue;

      LColumn := Column(LAttrib);
      if Result = nil then
        Result := TColumnMappingList.Create;
      Result.Add(TColumnMapping.Create);
      Result.Last.ColumnName := LColumn.ColumnName;
      Result.Last.FieldType := LColumn.FieldType;
      Result.Last.Scale := LColumn.Scale;
      Result.Last.Size := LColumn.Size;
      Result.Last.Precision := LColumn.Precision;
      Result.Last.ColumnProperty := LProperty;
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
      Result.Last.IsNullable := LProperty.IsNullable;
      Result.Last.IsVirtualData := LProperty.IsVirtualData;
      Result.Last.DefaultValue := '';
      Result.Last.ColumnDictionary := LProperty.GetDictionary;

      if Result.Last.ColumnDictionary <> nil then
        Result.Last.DefaultValue := Result.Last.ColumnDictionary.DefaultExpression
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
      if not (LAttrib is Enumeration) then // Enumeration
        Continue;

      if Result = nil then
         Result := TEnumerationMappingList.Create;
      Result.Add(TEnumerationMapping.Create(LProperty.PropertyType.AsOrdinal,
                                            Enumeration(LAttrib).EnumType,
                                            Enumeration(LAttrib).EnumValues));
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
      if not (LAttrib is FieldEvents) then // FieldEvents
        Continue;

      if Result = nil then
         Result := TFieldEventsMappingList.Create;
      Result.Add(TFieldEventsMapping.Create(Column(LProperty.GetColumn).ColumnName,
                                            FieldEvents(LAttrib).Events));
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
    if not (LAttrib is ForeignKey) then // ForeignKey
      Continue;

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
  /// Atributos das propriedades
  for LProperty in ARttiType.GetProperties do
  begin
    for LAttrib in LProperty.GetAttributes do
    begin
      if not (LAttrib is ForeignKey) then // ForeignKey
        Continue;

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

function TMappingPopular.PopularIndexe(ARttiType: TRttiType): TIndexeMappingList;
var
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  for LAttrib in ARttiType.GetAttributes do
  begin
    if not (LAttrib is Indexe) then // Indexe
      Continue;

    if Result = nil then
       Result := TIndexeMappingList.Create;
    Result.Add(TIndexeMapping.Create(Indexe(LAttrib).Name,
                                     Indexe(LAttrib).Columns,
                                     Indexe(LAttrib).SortingOrder,
                                     Indexe(LAttrib).Unique,
                                     Indexe(LAttrib).Description));
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
      if not (LAttrib is JoinColumn) then // JoinColumn
        Continue;

      if Length(JoinColumn(LAttrib).RefTableName) = 0 then
        Continue;

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

function TMappingPopular.PopularOrderBy(ARttiType: TRttiType): TOrderByMapping;
var
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  for LAttrib in ARttiType.GetAttributes do
  begin
    if not (LAttrib is OrderBy) then // OrderBy
      Continue;

    Result := TOrderByMapping.Create;
    Result.ColumnsName := OrderBy(LAttrib).ColumnsName;
  end;
end;

function TMappingPopular.PopularPrimaryKey(
  ARttiType: TRttiType): TPrimaryKeyMapping;
var
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  for LAttrib in ARttiType.GetAttributes do
  begin
    if not (LAttrib is PrimaryKey) then // PrimaryKey
      Continue;
    if Result <> nil then
      raise Exception.Create('There must be only one PrimaryKey() attribute in your model class.');
    Result := TPrimaryKeyMapping.Create(PrimaryKey(LAttrib).Columns,
                                        PrimaryKey(LAttrib).SequenceType = AutoInc,
                                        PrimaryKey(LAttrib).SequenceType = TableInc,
                                        PrimaryKey(LAttrib).SequenceType = GuidInc,
                                        PrimaryKey(LAttrib).SortingOrder,
                                        PrimaryKey(LAttrib).Unique,
                                        PrimaryKey(LAttrib).Description);
  end;
end;

function TMappingPopular.PopularPrimaryKeyColumns(
  ARttiType: TRttiType; AClass: TClass): TPrimaryKeyColumnsMapping;
var
  LColumns: TColumnMappingList;
  LColumn: TColumnMapping;
begin
  LColumns := TMappingExplorer.GetInstance.GetMappingColumn(AClass);
  if LColumns = nil then
    Exit(nil);

  Result := TPrimaryKeyColumnsMapping.Create;
  for LColumn in LColumns do
  begin
    if not LColumn.IsPrimaryKey then
      Continue;

    Result.Columns.Add(LColumn);
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

    if not (LAttrib is Sequence) then // Sequence
      Continue;

    Result := TSequenceMapping.Create;
    Result.Name := Sequence(LAttrib).Name;
    Result.Initial := Sequence(LAttrib).Initial;
    Result.Increment := Sequence(LAttrib).Increment;
  end;
  if (Result <> nil) and (LTable <> nil) then
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
      SetLength(LColumns, 0);
      if not (LAssociation is Association) then // Association
        Continue;

      if Length(Association(LAssociation).ColumnsNameRef) = 0 then
        Continue;

      if Result = nil then
         Result := TAssociationMappingList.Create;

      LRttiType := LProperty.PropertyType;
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

function TMappingPopular.PopularTable(ARttiType: TRttiType): TTableMapping;
var
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  for LAttrib in ARttiType.GetAttributes do
  begin
    if not ((LAttrib is Table) or (LAttrib is View)) then // Table
      Continue;

    Result := TTableMapping.Create;
    Result.Name := Table(LAttrib).Name;
    Result.Description := Table(LAttrib).Description;
    Result.Schema := '';
  end;
end;

function TMappingPopular.PopularTrigger(ARttiType: TRttiType): TTriggerMappingList;
var
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  for LAttrib in ARttiType.GetAttributes do
  begin
    if not (LAttrib is Trigger) then // Trigger
      Continue;

    if Result = nil then
       Result := TTriggerMappingList.Create;
    Result.Add(TTriggerMapping.Create(Trigger(LAttrib).Name,
                                      ''));
  end;
end;

function TMappingPopular.PopularView(ARttiType: TRttiType): TViewMapping;
var
  LAttrib: TCustomAttribute;
begin
  Result := nil;
  for LAttrib in ARttiType.GetAttributes do
  begin
    if not (LAttrib is View) then // View
      Continue;

    Result := TViewMapping.Create;
    Result.Name := View(LAttrib).Name;
    Result.Description := View(LAttrib).Description;
    Result.Script := '';
  end;
end;

end.


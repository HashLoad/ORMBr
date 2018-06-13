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

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.metadata.mysql;

interface

uses
  DB,
  SysUtils,
  Variants,
  Generics.Collections,
  ormbr.metadata.register,
  ormbr.metadata.extract,
  ormbr.database.mapping,
  ormbr.factory.interfaces,
  ormbr.types.database;

type
  TCatalogMetadataMySQL = class(TCatalogMetadataAbstract)
  protected
    function GetSelectTables: string; override;
    function GetSelectTableColumns(ATableName: string): string; override;
    function GetSelectPrimaryKey(ATableName: string): string; override;
    function GetSelectPrimaryKeyColumns(APrimaryKeyName: string): string; override;
    function GetSelectForeignKey(ATableName: string): string; override;
    function GetSelectForeignKeyColumns(AForeignKeyName: string): string; overload;
    function GetSelectIndexe(ATableName: string): string; override;
    function GetSelectIndexeColumns(AIndexeName: string): string; override;
    function GetSelectTriggers(ATableName: string): string; override;
    function GetSelectViews: string; override;
    function GetSelectSequences: string; override;
    function Execute: IDBResultSet;
  public
    procedure CreateFieldTypeList; override;
    procedure GetCatalogs; override;
    procedure GetSchemas; override;
    procedure GetTables; override;
    procedure GetColumns(ATable: TTableMIK); override;
    procedure GetPrimaryKey(ATable: TTableMIK); override;
    procedure GetIndexeKeys(ATable: TTableMIK); override;
    procedure GetForeignKeys(ATable: TTableMIK); override;
    procedure GetTriggers(ATable: TTableMIK); override;
    procedure GetChecks(ATable: TTableMIK); override;
    procedure GetSequences; override;
    procedure GetProcedures; override;
    procedure GetFunctions; override;
    procedure GetViews; override;
    procedure GetDatabaseMetadata; override;
  end;

implementation

{ TSchemaExtractSQLite }

procedure TCatalogMetadataMySQL.CreateFieldTypeList;
begin
  if Assigned(FFieldType) then
  begin
    FFieldType.Clear;
    FFieldType.Add('BIGINT', ftLargeint);
    FFieldType.Add('BINARY', ftSmallint);
    FFieldType.Add('BIT', ftSmallint);
    FFieldType.Add('BLOB', ftMemo);
    FFieldType.Add('BOOLEAN', ftBoolean);
    FFieldType.Add('CHAR', ftFixedChar);
    FFieldType.Add('CARACTER', ftFixedChar);
    FFieldType.Add('DATE', ftDate);
    FFieldType.Add('DATETIME', ftDateTime);
    FFieldType.Add('DECIMAL', ftBCD);
    FFieldType.Add('DOUBLE PRECISION', ftExtended);
    FFieldType.Add('FLOAT', ftFloat);
    FFieldType.Add('FLOAT4', ftFloat);
    FFieldType.Add('FLOAT8', ftFloat);
    FFieldType.Add('INT', ftInteger);
    FFieldType.Add('INTEGER', ftInteger);
    FFieldType.Add('INT1', ftSmallint);
    FFieldType.Add('INT2', ftSmallint);
    FFieldType.Add('NCHAR', ftFixedWideChar);
    FFieldType.Add('NVARCHAR', ftWideString);
    FFieldType.Add('VARCHAR', ftString);
    FFieldType.Add('REAL', ftFloat);
    FFieldType.Add('NUMERIC', ftFloat);
    FFieldType.Add('TEXT', ftWideMemo);
    FFieldType.Add('SMALLINT', ftSmallint);
    FFieldType.Add('TIMESTAMP', ftTimeStamp);
    FFieldType.Add('TIME', ftTime);
  end;
end;

function TCatalogMetadataMySQL.Execute: IDBResultSet;
var
  oSQLQuery: IDBQuery;
begin
  inherited;
  oSQLQuery := FConnection.CreateQuery;
  try
    oSQLQuery.CommandText := FSQLText;
    Exit(oSQLQuery.ExecuteQuery);
  except
    raise
  end;
end;

procedure TCatalogMetadataMySQL.GetDatabaseMetadata;
begin
  inherited;
  GetCatalogs;
end;

procedure TCatalogMetadataMySQL.GetCatalogs;
begin
  inherited;
  FCatalogMetadata.Name := '';
  GetSchemas;
end;

procedure TCatalogMetadataMySQL.GetChecks(ATable: TTableMIK);
begin
  /// Not Suported.
end;

procedure TCatalogMetadataMySQL.GetSchemas;
begin
  inherited;
  FCatalogMetadata.Schema := '';
  GetSequences;
  GetTables;
end;

procedure TCatalogMetadataMySQL.GetTables;
var
  oDBResultSet: IDBResultSet;
  oTable: TTableMIK;
begin
  inherited;
  FSQLText := GetSelectTables;
  oDBResultSet := Execute;
  while oDBResultSet.NotEof do
  begin
    oTable := TTableMIK.Create(FCatalogMetadata);
    oTable.Name := VarToStr(oDBResultSet.GetFieldValue('table_name'));
    oTable.Description := VarToStr(oDBResultSet.GetFieldValue('table_description'));
    /// <summary>
    /// Extrair colunas da tabela
    /// </summary>
    GetColumns(oTable);
    /// <summary>
    /// Extrair Primary Key da tabela
    /// </summary>
    GetPrimaryKey(oTable);
    /// <summary>
    /// Extrair Foreign Keys da tabela
    /// </summary>
    GetForeignKeys(oTable);
    /// <summary>
    /// Extrair Indexes da tabela
    /// </summary>
    GetIndexeKeys(oTable);
    /// <summary>
    /// Extrair Checks da tabela
    /// </summary>
    GetChecks(oTable);
    /// <summary>
    /// Adiciona na lista de tabelas extraidas
    /// </summary>
    FCatalogMetadata.Tables.Add(UpperCase(oTable.Name), oTable);
  end;
end;

procedure TCatalogMetadataMySQL.GetColumns(ATable: TTableMIK);
var
  oDBResultSet: IDBResultSet;
  oColumn: TColumnMIK;

  function ExtractDefaultValue(ADefaultValue: Variant): string;
  begin
    Result := '';
    if ADefaultValue <> Null then
      Result := ADefaultValue;
    if Result = '0.000' then
      Result := '0';
  end;

  function ResolveIntegerNullValue(AValue: Variant): Integer;
  begin
    Result := 0;
    if AValue <> Null then
    begin
      try
        Result := VarAsType(AValue, varInteger);
      except
        on E: Exception do
        begin
          raise Exception.Create(E.Message + sLineBreak +
                                 'Table: ' + ATable.Name + sLineBreak +
                                 'Column: ' + oColumn.Name);
        end;
      end;
    end;
  end;

begin
  inherited;
  FSQLText := GetSelectTableColumns(ATable.Name);
  oDBResultSet := Execute;
  while oDBResultSet.NotEof do
  begin
    oColumn := TColumnMIK.Create(ATable);
    oColumn.Name := VarToStr(oDBResultSet.GetFieldValue('column_name'));
    oColumn.Position := VarAsType(oDBResultSet.GetFieldValue('column_position'), varInteger) -1;
    oColumn.Size := ResolveIntegerNullValue(oDBResultSet.GetFieldValue('column_size'));
    oColumn.Precision := ResolveIntegerNullValue(oDBResultSet.GetFieldValue('column_precision'));
    oColumn.Scale := ResolveIntegerNullValue(oDBResultSet.GetFieldValue('column_scale'));
    oColumn.NotNull := VarToStr(oDBResultSet.GetFieldValue('column_nullable')) = 'NO';
    oColumn.DefaultValue := ExtractDefaultValue(VarToStr(oDBResultSet.GetFieldValue('column_defaultvalue')));
    oColumn.Description := VarToStr(oDBResultSet.GetFieldValue('column_description'));
    oColumn.TypeName := VarToStr(oDBResultSet.GetFieldValue('column_typename'));
    SetFieldType(oColumn);
    /// <summary>
    /// Resolve Field Type
    /// </summary>
    GetFieldTypeDefinition(oColumn);
    ATable.Fields.Add(FormatFloat('000000', oColumn.Position), oColumn);
  end;
end;

procedure TCatalogMetadataMySQL.GetPrimaryKey(ATable: TTableMIK);
var
  oDBResultSet: IDBResultSet;

  procedure GetPrimaryKeyColumns(APrimaryKey: TPrimaryKeyMIK);
  var
    oDBResultSet: IDBResultSet;
    oColumn: TColumnMIK;
  begin
    FSQLText := GetSelectPrimaryKeyColumns(APrimaryKey.Table.Name);
    oDBResultSet := Execute;
    while oDBResultSet.NotEof do
    begin
      oColumn := TColumnMIK.Create(ATable);
      oColumn.Name := VarToStr(oDBResultSet.GetFieldValue('column_name'));
      oColumn.Position := VarAsType(oDBResultSet.GetFieldValue('column_position'), varInteger) -1;
      oColumn.NotNull := True;
      APrimaryKey.Fields.Add(FormatFloat('000000', oColumn.Position), oColumn);
    end;
  end;

begin
  inherited;
  FSQLText := GetSelectPrimaryKey(ATable.Name);
  oDBResultSet := Execute;
  while oDBResultSet.NotEof do
  begin
    ATable.PrimaryKey.Name := VarToStr(oDBResultSet.GetFieldValue('pk_name'));
    ATable.PrimaryKey.Description := VarToStr(oDBResultSet.GetFieldValue('pk_description'));
    /// <summary>
    /// Extrai as columnas da primary key
    /// </summary>
    GetPrimaryKeyColumns(ATable.PrimaryKey);
  end;
end;

procedure TCatalogMetadataMySQL.GetForeignKeys(ATable: TTableMIK);

  procedure GetForeignKeyColumns(AForeignKey: TForeignKeyMIK);
  var
    oDBResultSet: IDBResultSet;
    oFromField: TColumnMIK;
    oToField: TColumnMIK;
  begin
//    FSQLText := GetSelectForeignKeyColumns(AForeignKey.Name);
    oDBResultSet := Execute;
    while oDBResultSet.NotEof do
    begin
      /// <summary>
      /// Coluna tabela source
      /// </summary>
      oFromField := TColumnMIK.Create(ATable);
      oFromField.Name := VarToStr(oDBResultSet.GetFieldValue('column_name'));
      oFromField.Position := VarAsType(oDBResultSet.GetFieldValue('column_position'), varInteger) -1;
      AForeignKey.FromFields.Add(FormatFloat('000000', oFromField.Position), oFromField);
      /// <summary>
      /// Coluna tabela referencia
      /// </summary>
      oToField := TColumnMIK.Create(ATable);
      oToField.Name := VarToStr(oDBResultSet.GetFieldValue('column_reference'));
      oToField.Position := VarAsType(oDBResultSet.GetFieldValue('column_referenceposition'), varInteger) -1;
      AForeignKey.ToFields.Add(FormatFloat('000000', oToField.Position), oToField);
    end;
  end;

var
  oDBResultSet: IDBResultSet;
  oForeignKey: TForeignKeyMIK;
  oFromField: TColumnMIK;
  oToField: TColumnMIK;
begin
  inherited;
  FSQLText := GetSelectForeignKey(ATable.Name);
  oDBResultSet := Execute;
  while oDBResultSet.NotEof do
  begin
    oForeignKey := TForeignKeyMIK.Create(ATable);
    oForeignKey.Name := VarToStr(oDBResultSet.GetFieldValue('fk_name'));
    oForeignKey.FromTable := VarToStr(oDBResultSet.GetFieldValue('table_reference'));
    oForeignKey.OnUpdate := GetRuleAction(VarAsType(oDBResultSet.GetFieldValue('fk_updateaction'), varInteger));
    oForeignKey.OnDelete := GetRuleAction(VarAsType(oDBResultSet.GetFieldValue('fk_deleteaction'), varInteger));
    oForeignKey.Description :=  VarToStr(oDBResultSet.GetFieldValue('fk_description'));
    ATable.ForeignKeys.Add(oForeignKey.Name, oForeignKey);
    /// <summary>
    /// Coluna tabela master
    /// </summary>
    oFromField := TColumnMIK.Create(ATable);
    oFromField.Name := VarToStr(oDBResultSet.GetFieldValue('column_name'));
    oForeignKey.FromFields.Add(oFromField.Name, oFromField);
    /// <summary>
    /// Coluna tabela referencia
    /// </summary>
    oToField := TColumnMIK.Create(ATable);
    oToField.Name := VarToStr(oDBResultSet.GetFieldValue('column_reference'));
    oForeignKey.ToFields.Add(oToField.Name, oToField);
    /// <summary>
    /// Gera a lista de campos do foreignkey
    /// </summary>
//    GetForeignKeyColumns(oForeignKey);
  end;
end;

procedure TCatalogMetadataMySQL.GetFunctions;
begin
  inherited;

end;

procedure TCatalogMetadataMySQL.GetProcedures;
begin
  inherited;

end;

procedure TCatalogMetadataMySQL.GetSequences;
var
  oDBResultSet: IDBResultSet;
  oSequence: TSequenceMIK;
begin
  inherited;
  FSQLText := GetSelectSequences;
  oDBResultSet := Execute;
  while oDBResultSet.NotEof do
  begin
    oSequence := TSequenceMIK.Create(FCatalogMetadata);
    oSequence.TableName := VarToStr(oDBResultSet.GetFieldValue('name'));
    oSequence.Name := VarToStr(oDBResultSet.GetFieldValue('name'));
    oSequence.Description := VarToStr(oDBResultSet.GetFieldValue('description'));
    FCatalogMetadata.Sequences.Add(UpperCase(oSequence.Name), oSequence);
  end;
end;

procedure TCatalogMetadataMySQL.GetTriggers(ATable: TTableMIK);
var
  oDBResultSet: IDBResultSet;
  oTrigger: TTriggerMIK;
begin
  inherited;
  FSQLText := '';
  oDBResultSet := Execute;
  while oDBResultSet.NotEof do
  begin
    oTrigger := TTriggerMIK.Create(ATable);
    oTrigger.Name := VarToStr(oDBResultSet.GetFieldValue('name'));
    oTrigger.Description := '';
    oTrigger.Script := VarToStr(oDBResultSet.GetFieldValue('sql'));
    ATable.Triggers.Add(UpperCase(oTrigger.Name), oTrigger);
  end;
end;

procedure TCatalogMetadataMySQL.GetIndexeKeys(ATable: TTableMIK);
var
  oDBResultSet: IDBResultSet;
  oIndexeKey: TIndexeKeyMIK;

  procedure GetIndexeKeyColumns(AIndexeKey: TIndexeKeyMIK);
  var
    oDBResultSet: IDBResultSet;
    oColumn: TColumnMIK;
  begin
    FSQLText := GetSelectIndexeColumns(AIndexeKey.Name);
    oDBResultSet := Execute;
    while oDBResultSet.NotEof do
    begin
      oColumn := TColumnMIK.Create(ATable);
      oColumn.Name := VarToStr(oDBResultSet.GetFieldValue('column_name'));
      oColumn.Position := VarAsType(oDBResultSet.GetFieldValue('column_position'), varInteger) -1;
      AIndexeKey.Fields.Add(FormatFloat('000000', oColumn.Position), oColumn);
    end;
  end;

begin
  inherited;
  FSQLText := GetSelectIndexe(ATable.Name);
  oDBResultSet := Execute;
  while oDBResultSet.NotEof do
  begin
    oIndexeKey := TIndexeKeyMIK.Create(ATable);
    oIndexeKey.Name := VarToStr(oDBResultSet.GetFieldValue('indexe_name'));
    oIndexeKey.Unique := VarAsType(oDBResultSet.GetFieldValue('indexe_unique'), varInteger) = 0;
    ATable.IndexeKeys.Add(UpperCase(oIndexeKey.Name), oIndexeKey);
    /// <summary>
    /// Gera a lista de campos do indexe
    /// </summary>
    GetIndexeKeyColumns(oIndexeKey);
  end;
end;

procedure TCatalogMetadataMySQL.GetViews;
var
  oDBResultSet: IDBResultSet;
  oView: TViewMIK;
begin
  inherited;
  FSQLText := GetSelectViews;
  oDBResultSet := Execute;
  while oDBResultSet.NotEof do
  begin
    oView := TViewMIK.Create(FCatalogMetadata);
    oView.Name := VarToStr(oDBResultSet.GetFieldValue('view_name'));
    oView.Script := VarToStr(oDBResultSet.GetFieldValue('view_script'));
    oView.Description := VarToStr(oDBResultSet.GetFieldValue('view_description'));
    FCatalogMetadata.Views.Add(UpperCase(oView.Name), oView);
  end;
end;

function TCatalogMetadataMySQL.GetSelectPrimaryKey(ATableName: string): string;
begin
   Result := ' select rc.constraint_name as pk_name, ' +
             '        ''''               as pk_description ' +
             ' from information_schema.table_constraints rc ' +
             ' where (rc.constraint_type = ''PRIMARY KEY'') ' +
             ' and   (rc.table_name = ' + QuotedStr(ATableName) + ')' +
             ' order by rc.constraint_name ';
end;

function TCatalogMetadataMySQL.GetSelectPrimaryKeyColumns(APrimaryKeyName: string): string;
begin
   Result := ' select c.column_name       as column_name, ' +
             '        c.ordinal_position  as column_position ' +
             ' from information_schema.key_column_usage c ' +
             ' inner join information_schema.table_constraints t on c.table_name = t.table_name ' +
             '                                                  and c.constraint_schema = database() ' +
             '                                                  and t.constraint_schema = database() ' +
             '                                                  and t.constraint_name = c.constraint_name ' +
             '                                                  and t.constraint_type = ''PRIMARY KEY'' ' +
             ' where t.table_name = ' + QuotedStr(APrimaryKeyName) +
             ' order by t.table_name, ' +
             '          t.constraint_name, ' +
             '          c.ordinal_position ';
end;

function TCatalogMetadataMySQL.GetSelectSequences: string;
begin
  Result :=  ' select table_name    as name, ' +
             '        table_comment as description ' +
             ' from information_schema.tables ' +
             ' where table_schema = database() ' +
             ' and auto_increment is not null';
end;

function TCatalogMetadataMySQL.GetSelectTableColumns(ATableName: string): string;
begin
   Result := ' select column_name              as column_name, ' +
             '        ordinal_position         as column_position, ' +
             '        character_maximum_length as column_size, ' +
             '        numeric_precision        as column_precision, ' +
             '        numeric_scale            as column_scale, ' +
             '        collation_name           as column_collation, ' +
             '        is_nullable              as column_nullable, ' +
             '        column_default           as column_defaultvalue, ' +
             '        column_comment           as column_description, ' +
             '  upper(data_type)               as column_typename, ' +
             '        character_set_name       as column_charset ' +
             ' from information_schema.columns ' +
             ' where table_name in (' + QuotedStr(ATableName) + ') ' +
             ' order by ordinal_position' ;
end;

function TCatalogMetadataMySQL.GetSelectTables: string;
begin
  Result := ' select table_name as table_name, ' +
            '        ''''       as table_description ' +
            ' from information_schema.tables ' +
            ' where table_type = ''BASE TABLE'' ' +
            ' and table_schema = database() ' +
            ' order by table_name';
end;

function TCatalogMetadataMySQL.GetSelectTriggers(ATableName: string): string;
begin
  Result := '';
end;

function TCatalogMetadataMySQL.GetSelectForeignKey(ATableName: string): string;
begin
  Result := ' select rc.constraint_name               as fk_name, ' +
            '        rc.update_rule                   as fk_updateaction, ' +
            '        rc.delete_rule                   as fk_deleteaction, ' +
            '        kc.column_name                   as column_name, ' +
            '        kc.ordinal_position              as column_position, ' +
            '        rc.referenced_table_name         as table_reference, ' +
            '        kc.referenced_column_name        as column_reference, ' +
            '        kc.position_in_unique_constraint as column_referenceposition, ' +
            '        ''''                             as fk_description ' +
            ' from information_schema.key_column_usage kc ' +
            '  inner join information_schema.referential_constraints rc on kc.constraint_name = rc.constraint_name ' +
            '                                                          and kc.table_name = rc.table_name ' +
            '                                                          and kc.constraint_schema = database() ' +
            '                                                          and rc.constraint_schema = database() ' +
            ' where rc.table_name in(' + QuotedStr(ATableName) + ') ' +
            ' order by rc.constraint_name, kc.position_in_unique_constraint';
end;

function TCatalogMetadataMySQL.GetSelectForeignKeyColumns(AForeignKeyName: string): string;
begin
  Result := 'Falta Implementar';
end;

function TCatalogMetadataMySQL.GetSelectViews: string;
begin
   Result := ' select v.table_name      as view_name, ' +
             '        v.view_definition as view_script, ' +
             '        ''''              as view_description' +
             ' from information_schema.views v';
end;

function TCatalogMetadataMySQL.GetSelectIndexe(ATableName: string): string;
begin
  Result := ' select idx.index_name as indexe_name, ' +
            '        idx.non_unique as indexe_unique, ' +
            '        idx.comment    as indexe_description ' +
            ' from information_schema.statistics idx ' +
            ' where idx.index_name not in (''PRIMARY'') ' +
            ' and   idx.table_name in(' + QuotedStr(ATableName) + ') ' +
            ' order by idx.table_name, idx.index_name';
end;

function TCatalogMetadataMySQL.GetSelectIndexeColumns(AIndexeName: string): string;
begin
   Result := ' select idx.column_name  as column_name, ' +
             '        idx.seq_in_index as column_position, ' +
             '        idx.comment      as column_description ' +
             ' from information_schema.statistics idx ' +
             ' where idx.index_name in(' + QuotedStr(AIndexeName) + ') ' +
             ' order by idx.index_name, idx.seq_in_index';
end;

initialization
  TMetadataRegister.GetInstance.RegisterMetadata(dnMySQL, TCatalogMetadataMySQL.Create);

end.

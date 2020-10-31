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

unit ormbr.metadata.postgresql;

interface

uses
  SysUtils,
  Variants,
  DB,
  Generics.Collections,
  ormbr.metadata.register,
  ormbr.metadata.extract,
  ormbr.database.mapping,
  dbebr.factory.interfaces;

type
  TCatalogMetadataPostgreSQL = class(TCatalogMetadataAbstract)
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
    function GetSelectChecks(ATableName: string): string; override;
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

procedure TCatalogMetadataPostgreSQL.CreateFieldTypeList;
begin
  if Assigned(FFieldType) then
  begin
    FFieldType.Clear;
    FFieldType.Add('VARCHAR', ftString);
    FFieldType.Add('REAL', ftFloat);
    FFieldType.Add('DOUBLE PRECISION', ftExtended);
    FFieldType.Add('CHAR', ftFixedChar);
    FFieldType.Add('DECIMAL', ftBCD);
    FFieldType.Add('NUMERIC', ftFloat);
    FFieldType.Add('TEXT', ftWideMemo);
    FFieldType.Add('SMALLINT', ftSmallint);
    FFieldType.Add('INT', ftInteger);
    FFieldType.Add('BIGINT', ftLargeint);
    FFieldType.Add('INT4', ftInteger);
    FFieldType.Add('INT8', ftLargeint);
    FFieldType.Add('BYTEA', ftBlob);
    FFieldType.Add('BOOL', ftBoolean);
    FFieldType.Add('TIMESTAMP', ftTimeStamp);
    FFieldType.Add('DATE', ftDate);
    FFieldType.Add('TIME', ftTime);
  end;
end;

function TCatalogMetadataPostgreSQL.Execute: IDBResultSet;
var
  LSQLQuery: IDBQuery;
begin
  inherited;
  LSQLQuery := FConnection.CreateQuery;
  try
    LSQLQuery.CommandText := FSQLText;
    Exit(LSQLQuery.ExecuteQuery);
  except
    raise
  end;
end;

procedure TCatalogMetadataPostgreSQL.GetDatabaseMetadata;
begin
  inherited;
  GetCatalogs;
end;

procedure TCatalogMetadataPostgreSQL.GetCatalogs;
begin
  inherited;
  FCatalogMetadata.Name := '';
  GetSchemas;
end;

procedure TCatalogMetadataPostgreSQL.GetChecks(ATable: TTableMIK);
var
  LDBResultSet: IDBResultSet;
  LCheck: TCheckMIK;
begin
  inherited;
  FSQLText := GetSelectChecks(ATable.Name);
  LDBResultSet := Execute;
  while LDBResultSet.NotEof do
  begin
    LCheck := TCheckMIK.Create(ATable);
    LCheck.Name := VarToStr(LDBResultSet.GetFieldValue('name'));
    LCheck.Description := '';
    LCheck.Condition := VarToStr(LDBResultSet.GetFieldValue('condition'));
    ATable.Checks.Add(UpperCase(LCheck.Name), LCheck);
  end;
end;

procedure TCatalogMetadataPostgreSQL.GetSchemas;
begin
  inherited;
  FCatalogMetadata.Schema := 'public';
  GetSequences;
  GetTables;
end;

procedure TCatalogMetadataPostgreSQL.GetTables;
var
  LDBResultSet: IDBResultSet;
  LTable: TTableMIK;
begin
  inherited;
  FSQLText := GetSelectTables;
  LDBResultSet := Execute;
  while LDBResultSet.NotEof do
  begin
    LTable := TTableMIK.Create(FCatalogMetadata);
    LTable.Name := VarToStr(LDBResultSet.GetFieldValue('table_name'));
    LTable.Description := VarToStr(LDBResultSet.GetFieldValue('table_description'));
    /// <summary>
    /// Extrair colunas da tabela
    /// </summary>
    GetColumns(LTable);
    /// <summary>
    /// Extrair Primary Key da tabela
    /// </summary>
    GetPrimaryKey(LTable);
    /// <summary>
    /// Extrair Foreign Keys da tabela
    /// </summary>
    GetForeignKeys(LTable);
    /// <summary>
    /// Extrair Indexes da tabela
    /// </summary>
    GetIndexeKeys(LTable);
    /// <summary>
    /// Extrair Checks da tabela
    /// </summary>
    GetChecks(LTable);
    /// <summary>
    /// Adiciona na lista de tabelas extraidas
    /// </summary>
    FCatalogMetadata.Tables.Add(UpperCase(LTable.Name), LTable);
  end;
end;

procedure TCatalogMetadataPostgreSQL.GetColumns(ATable: TTableMIK);
var
  LDBResultSet: IDBResultSet;
  LColumn: TColumnMIK;

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
      Result := VarAsType(AValue, varInteger);
  end;

begin
  inherited;
  FSQLText := GetSelectTableColumns(ATable.Name);
  LDBResultSet := Execute;
  while LDBResultSet.NotEof do
  begin
    LColumn := TColumnMIK.Create(ATable);
    LColumn.Name := VarToStr(LDBResultSet.GetFieldValue('column_name'));
    LColumn.Position := VarAsType(LDBResultSet.GetFieldValue('column_position'), varInteger) -1;
    LColumn.Size := ResolveIntegerNullValue(LDBResultSet.GetFieldValue('column_size'));
    LColumn.Precision := ResolveIntegerNullValue(LDBResultSet.GetFieldValue('column_precision'));
    LColumn.Scale := ResolveIntegerNullValue(LDBResultSet.GetFieldValue('column_scale'));
    LColumn.NotNull := VarToStr(LDBResultSet.GetFieldValue('column_nullable')) = 'NO';
    LColumn.DefaultValue := ExtractDefaultValue(VarToStr(LDBResultSet.GetFieldValue('column_defaultvalue')));
    LColumn.Description := VarToStr(LDBResultSet.GetFieldValue('column_description'));
    LColumn.TypeName := VarToStr(LDBResultSet.GetFieldValue('column_typename'));
    SetFieldType(LColumn);
    /// <summary>
    /// Resolve Field Type
    /// </summary>
    GetFieldTypeDefinition(LColumn);
    ATable.Fields.Add(FormatFloat('000000', LColumn.Position), LColumn);
  end;
end;

procedure TCatalogMetadataPostgreSQL.GetPrimaryKey(ATable: TTableMIK);
var
  LDBResultSet: IDBResultSet;

  procedure GetPrimaryKeyColumns(APrimaryKey: TPrimaryKeyMIK);
  var
    LDBResultSet: IDBResultSet;
    LColumn: TColumnMIK;
  begin
    FSQLText := GetSelectPrimaryKeyColumns(APrimaryKey.Table.Name);
    LDBResultSet := Execute;
    while LDBResultSet.NotEof do
    begin
      LColumn := TColumnMIK.Create(ATable);
      LColumn.Name := VarToStr(LDBResultSet.GetFieldValue('column_name'));
      LColumn.Position := VarAsType(LDBResultSet.GetFieldValue('column_position'), varInteger) -1;
      LColumn.NotNull := True;
      APrimaryKey.Fields.Add(FormatFloat('000000', LColumn.Position), LColumn);
    end;
  end;

begin
  inherited;
  FSQLText := GetSelectPrimaryKey(ATable.Name);
  LDBResultSet := Execute;
  while LDBResultSet.NotEof do
  begin
    ATable.PrimaryKey.Name := Format('PK_%s', [ATable.Name]);
    ATable.PrimaryKey.Description := VarToStr(LDBResultSet.GetFieldValue('pk_description'));
    /// <summary>
    /// Extrai as columnas da primary key
    /// </summary>
    GetPrimaryKeyColumns(ATable.PrimaryKey);
  end;
end;

procedure TCatalogMetadataPostgreSQL.GetForeignKeys(ATable: TTableMIK);

  procedure GetForeignKeyColumns(AForeignKey: TForeignKeyMIK);
  var
    LDBResultSet: IDBResultSet;
    LFromField: TColumnMIK;
    LToField: TColumnMIK;
  begin
//    FSQLText := GetSelectForeignKeyColumns(AForeignKey.Name);
    LDBResultSet := Execute;
    while LDBResultSet.NotEof do
    begin
      /// <summary>
      /// Coluna tabela source
      /// </summary>
      LFromField := TColumnMIK.Create(ATable);
      LFromField.Name := VarToStr(LDBResultSet.GetFieldValue('column_name'));
      LFromField.Position := VarAsType(LDBResultSet.GetFieldValue('column_position'), varInteger) -1;
      AForeignKey.FromFields.Add(FormatFloat('000000', LFromField.Position), LFromField);
      /// <summary>
      /// Coluna tabela referencia
      /// </summary>
      LToField := TColumnMIK.Create(ATable);
      LToField.Name := VarToStr(LDBResultSet.GetFieldValue('column_reference'));
      LToField.Position := VarAsType(LDBResultSet.GetFieldValue('column_referenceposition'), varInteger) -1;
      AForeignKey.ToFields.Add(FormatFloat('000000', LToField.Position), LToField);
    end;
  end;

var
  LDBResultSet: IDBResultSet;
  LForeignKey: TForeignKeyMIK;
  LFromField: TColumnMIK;
  LToField: TColumnMIK;
begin
  inherited;
  FSQLText := GetSelectForeignKey(ATable.Name);
  LDBResultSet := Execute;
  while LDBResultSet.NotEof do
  begin
    LForeignKey := TForeignKeyMIK.Create(ATable);
//    LForeignKey.Name := Format('FK_%s_%s', [VarToStr(LDBResultSet.GetFieldValue('table_reference')),
//                                            VarToStr(LDBResultSet.GetFieldValue('column_name'))]);
    LForeignKey.Name := VarToStr(LDBResultSet.GetFieldValue('fk_name'));
    LForeignKey.FromTable := VarToStr(LDBResultSet.GetFieldValue('table_reference'));
//    LForeignKey.OnUpdate := GetRuleAction(VarAsType(LDBResultSet.GetFieldValue('fk_updateaction'), varInteger));
//    LForeignKey.OnDelete := GetRuleAction(VarAsType(LDBResultSet.GetFieldValue('fk_deleteaction'), varInteger));
    LForeignKey.OnUpdate := GetRuleAction(VarToStr(LDBResultSet.GetFieldValue('fk_updateaction')));
    LForeignKey.OnDelete := GetRuleAction(VarToStr(LDBResultSet.GetFieldValue('fk_deleteaction')));
    LForeignKey.Description :=  VarToStr(LDBResultSet.GetFieldValue('fk_description'));
    ATable.ForeignKeys.Add(LForeignKey.Name, LForeignKey);
    // Coluna tabela master
    LFromField := TColumnMIK.Create(ATable);
    LFromField.Name := VarToStr(LDBResultSet.GetFieldValue('column_name'));
    LForeignKey.FromFields.Add(LFromField.Name, LFromField);
    // Coluna tabela referencia
    LToField := TColumnMIK.Create(ATable);
    LToField.Name := VarToStr(LDBResultSet.GetFieldValue('column_reference'));
    LForeignKey.ToFields.Add(LToField.Name, LToField);
    // Gera a lista de campos do foreignkey
//    GetForeignKeyColumns(LForeignKey);
  end;
end;

procedure TCatalogMetadataPostgreSQL.GetFunctions;
begin
  inherited;

end;

procedure TCatalogMetadataPostgreSQL.GetProcedures;
begin
  inherited;

end;

procedure TCatalogMetadataPostgreSQL.GetSequences;
var
  LDBResultSet: IDBResultSet;
  LSequence: TSequenceMIK;
begin
  inherited;
  FSQLText := GetSelectSequences;
  LDBResultSet := Execute;
  while LDBResultSet.NotEof do
  begin
    LSequence := TSequenceMIK.Create(FCatalogMetadata);
    LSequence.Name := VarToStr(LDBResultSet.GetFieldValue('name'));
    LSequence.Description := VarToStr(LDBResultSet.GetFieldValue('description'));;
    FCatalogMetadata.Sequences.Add(UpperCase(LSequence.Name), LSequence);
  end;
end;

procedure TCatalogMetadataPostgreSQL.GetTriggers(ATable: TTableMIK);
var
  LDBResultSet: IDBResultSet;
  LTrigger: TTriggerMIK;
begin
  inherited;
  FSQLText := '';
  LDBResultSet := Execute;
  while LDBResultSet.NotEof do
  begin
    LTrigger := TTriggerMIK.Create(ATable);
    LTrigger.Name := VarToStr(LDBResultSet.GetFieldValue('name'));
    LTrigger.Description := '';
    LTrigger.Script := VarToStr(LDBResultSet.GetFieldValue('sql'));
    ATable.Triggers.Add(UpperCase(LTrigger.Name), LTrigger);
  end;
end;

procedure TCatalogMetadataPostgreSQL.GetIndexeKeys(ATable: TTableMIK);
var
  LDBResultSet: IDBResultSet;
  LIndexeKey: TIndexeKeyMIK;

  procedure GetIndexeKeyColumns(AIndexeKey: TIndexeKeyMIK);
  var
    LDBResultSet: IDBResultSet;
    LColumn: TColumnMIK;
    iColumn: Integer;
  begin
    FSQLText := GetSelectIndexeColumns(AIndexeKey.Name);
    LDBResultSet := Execute;
    while LDBResultSet.NotEof do
    begin
      LColumn := TColumnMIK.Create(ATable);
      LColumn.Name := VarToStr(LDBResultSet.GetFieldValue('column_name'));
      LColumn.Position := LColumn.Position + 1; //  VarAsType(LDBResultSet.GetFieldValue('column_position'), varInteger);
      AIndexeKey.Fields.Add(FormatFloat('000000', LColumn.Position), LColumn);
    end;
  end;

begin
  inherited;
  FSQLText := GetSelectIndexe(ATable.Name);
  LDBResultSet := Execute;
  while LDBResultSet.NotEof do
  begin
    LIndexeKey := TIndexeKeyMIK.Create(ATable);
    LIndexeKey.Name := VarToStr(LDBResultSet.GetFieldValue('indexe_name'));
    LIndexeKey.Unique := VarAsType(LDBResultSet.GetFieldValue('indexe_unique'), varBoolean);
    ATable.IndexeKeys.Add(UpperCase(LIndexeKey.Name), LIndexeKey);
    /// <summary>
    /// Gera a lista de campos do indexe
    /// </summary>
    GetIndexeKeyColumns(LIndexeKey);
  end;
end;

procedure TCatalogMetadataPostgreSQL.GetViews;
var
  LDBResultSet: IDBResultSet;
  LView: TViewMIK;
begin
  inherited;
  FSQLText := GetSelectViews;
  LDBResultSet := Execute;
  while LDBResultSet.NotEof do
  begin
    LView := TViewMIK.Create(FCatalogMetadata);
    LView.Name := VarToStr(LDBResultSet.GetFieldValue('view_name'));
    LView.Script := VarToStr(LDBResultSet.GetFieldValue('view_script'));
    LView.Description := VarToStr(LDBResultSet.GetFieldValue('view_description'));
    FCatalogMetadata.Views.Add(UpperCase(LView.Name), LView);
  end;
end;

function TCatalogMetadataPostgreSQL.GetSelectPrimaryKey(ATableName: string): string;
begin
   Result := ' select rc.constraint_name as pk_name, ' +
             '        ''''               as pk_description ' +
             ' from information_schema.table_constraints rc ' +
             ' where rc.constraint_type = ''PRIMARY KEY'' ' +
             ' and   rc.table_schema not in (''pg_catalog'', ''information_schema'') ' +
             ' and   rc.table_name in (' + QuotedStr(ATableName) + ') ' +
             ' order by rc.constraint_name ';
end;

function TCatalogMetadataPostgreSQL.GetSelectPrimaryKeyColumns(APrimaryKeyName: string): string;
begin
   Result := ' select c.column_name       as column_name, ' +
             '        c.ordinal_position  as column_position ' +
             ' from information_schema.key_column_usage c ' +
             ' inner join information_schema.table_constraints t on c.table_name = t.table_name ' +
             '                                                  and t.constraint_name = c.constraint_name ' +
             '                                                  and t.constraint_type = ''PRIMARY KEY'' ' +
             ' where t.table_name = ' + QuotedStr(APrimaryKeyName) +
             ' and   c.table_schema not in (''pg_catalog'', ''information_schema'') ' +
             ' order by t.table_name, ' +
             '          t.constraint_name, ' +
             '          c.ordinal_position ';
end;

function TCatalogMetadataPostgreSQL.GetSelectSequences: string;
begin
  Result := ' select relname as name, ' +
            '        ''''    as description ' +
            ' from pg_class ' +
            ' where relkind in (' + QuotedStr('S') + ')';
end;

function TCatalogMetadataPostgreSQL.GetSelectTableColumns(ATableName: string): string;
begin
   Result := ' select column_name              as column_name, ' +
             '        ordinal_position         as column_position, ' +
             '        character_maximum_length as column_size, ' +
             '        numeric_precision        as column_precision, ' +
             '        numeric_scale            as column_scale, ' +
             '        collation_name           as column_collation, ' +
             '        is_nullable              as column_nullable, ' +
             '        column_default           as column_defaultvalue, ' +
             '        ''''                     as column_description, ' +
             '  upper(udt_name)                as column_typename, ' +
             '        character_set_name       as column_charset ' +
             ' from information_schema.columns ' +
             ' where table_name in (' + QuotedStr(ATableName) + ') ' +
             ' order by ordinal_position' ;
end;

function TCatalogMetadataPostgreSQL.GetSelectTables: string;
begin
  Result := ' select table_name as table_name, ' +
            '        ''''       as table_description ' +
            ' from information_schema.tables ' +
            ' where table_type = ''BASE TABLE'' ' +
            ' and table_schema not in (''pg_catalog'', ''information_schema'') ' +
            ' order by table_name';
end;

function TCatalogMetadataPostgreSQL.GetSelectTriggers(ATableName: string): string;
begin
  Result := '';
end;

function TCatalogMetadataPostgreSQL.GetSelectForeignKey(ATableName: string): string;
begin
  Result := ' select kc.constraint_name               as fk_name, ' +
            '        kc.column_name                   as column_name, ' +
            '        kc.ordinal_position              as column_position, ' +
            '        ku.table_name                    as table_reference, ' +
            '        ku.column_name                   as column_reference, ' +
            '        kc.position_in_unique_constraint as column_referenceposition, ' +
            '        rc.update_rule                   as fk_updateaction, ' +
            '        rc.delete_rule                   as fk_deleteaction, ' +
            '        ''''                             as fk_description ' +
            ' from information_schema.key_column_usage kc ' +
            ' inner join information_schema.referential_constraints rc on kc.constraint_name = rc.constraint_name ' +
            ' inner join information_schema.key_column_usage ku on rc.unique_constraint_name = ku.constraint_name ' +
            ' where kc.table_name in(' + QuotedStr(ATableName) + ') ' +
            ' and   kc.table_schema not in (''pg_catalog'', ''information_schema'') ' +
            ' order by kc.constraint_name, kc.position_in_unique_constraint';
end;

function TCatalogMetadataPostgreSQL.GetSelectForeignKeyColumns(AForeignKeyName: string): string;
begin
  Result := 'Falta Implementar';
end;

function TCatalogMetadataPostgreSQL.GetSelectChecks(ATableName: string): string;
begin
  Result := ' select conname as name, ' +
            '        pg_get_constraintdef(c.oid) as condition ' +
            ' from pg_constraint c ' +
            ' inner join pg_namespace n on n.oid = c.connamespace ' +
            ' where conrelid::regclass = ' + QuotedStr(ATableName) + '::regclass' +
            '   and contype in (' + QuotedStr('c') + ')';
end;

function TCatalogMetadataPostgreSQL.GetSelectViews: string;
begin
   Result := ' select v.table_name      as view_name, ' +
             '        v.view_definition as view_script, ' +
             '        ''''              as view_description ' +
             ' from information_schema.views v ' +
             ' where table_schema not in (''pg_catalog'', ''information_schema'') ' +
             ' and table_name !~ ''^pg_'' ';
end;

function TCatalogMetadataPostgreSQL.GetSelectIndexe(ATableName: string): string;
begin
  Result := ' select c.relname     as indexe_name, ' +
            '        b.indisunique as indexe_unique, ' +
            '        ''''          as indexe_description ' +
            ' from pg_class as a ' +
            ' join pg_index as b on (a.oid = b.indrelid) ' +
            ' join pg_class as c on (c.oid = b.indexrelid) ' +
            ' where a.relname in(' + QuotedStr(ATableName) + ') ' +
            ' and   b.indisprimary != ''t'' ' +
            ' order by c.relname';
end;

function TCatalogMetadataPostgreSQL.GetSelectIndexeColumns(AIndexeName: string): string;
begin
  Result := ' select a.attname as column_name, ' +
            '        a.attnum  as column_position, ' +
            '        ''''      as column_description, ' +
            '        cast(c.indkey as varchar(20)) as column_ordem ' +
            ' from pg_index c ' +
            ' left join pg_class t on c.indrelid  = t.oid ' +
            ' left join pg_class i on c.indexrelid  = i.oid  ' +
            ' left join pg_attribute a on a.attrelid = t.oid and a.attnum = any(indkey) ' +
            ' where i.relname in(' + QuotedStr(AIndexeName) + ') ' +
            ' order by i.relname, a.attnum';
end;

initialization
  TMetadataRegister.GetInstance.RegisterMetadata(dnPostgreSQL, TCatalogMetadataPostgreSQL.Create);

end.

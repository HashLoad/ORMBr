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

unit ormbr.metadata.oracle;

interface

uses
  SysUtils,
  StrUtils,
  Variants,
  DB,
  Generics.Collections,
  ormbr.metadata.register,
  ormbr.metadata.extract,
  ormbr.database.mapping,
  ormbr.factory.interfaces,
  ormbr.types.mapping,
  ormbr.types.database;

type
  TCatalogMetadataOracle = class(TCatalogMetadataAbstract)
  protected
    procedure SetFieldType(var AColumnMIK: TColumnMIK); virtual;
    function GetSelectTables: string; override;
    function GetSelectTableColumns(ATableName: string): string; override;
    function GetSelectPrimaryKey(ATableName: string): string; override;
    function GetSelectPrimaryKeyColumns(APrimaryKeyName: string): string; override;
    function GetSelectForeignKey(ATableName: string): string; override;
    function GetSelectForeignKeyColumns(AForeignKeyName: string): string; overload;
    function GetSelectIndexe(ATableName: string): string; override;
    function GetSelectIndexeColumns(AIndexeName: string): string; override;
    function GetSelectTriggers(ATableName: string): string; override;
    function GetSelectChecks(ATableName: string): string; override;
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

procedure TCatalogMetadataOracle.CreateFieldTypeList;
begin
  if Assigned(FFieldType) then
  begin
    FFieldType.Clear;
    FFieldType.Add('CHAR', ftFixedChar);
    FFieldType.Add('BINARY_FLOAT', ftFloat);
    FFieldType.Add('BINARY_DOUBLE', ftBCD);
    FFieldType.Add('BOOLEAN', ftBoolean);
    FFieldType.Add('VARCHAR2', ftString);
    FFieldType.Add('NCHAR', ftFixedWideChar);
    FFieldType.Add('NVARCHAR', ftWideString);
    FFieldType.Add('NVARCHAR2', ftWideString);
    FFieldType.Add('NUMBER', ftCurrency);
    FFieldType.Add('DATE', ftDateTime);
    FFieldType.Add('CLOB', ftMemo);
    FFieldType.Add('NCLOB', ftWideMemo);
    FFieldType.Add('BLOB', ftOraBlob);
    FFieldType.Add('TIMESTAMP', ftTimeStamp);
    FFieldType.Add('TIMESTAMP(6)', ftTimeStamp);
    FFieldType.Add('FLOAT', ftFloat);
    FFieldType.Add('BFILE', ftWideString);
    FFieldType.Add('LONG', ftMemo);
    FFieldType.Add('UROWID', ftWideString);
    FFieldType.Add('ROWID', ftWideString);
    {
    FFieldType.Add('RAW', ftGuid);
    FFieldType.Add('LONG RAW', ftGuid);
    }
  end;
end;

function TCatalogMetadataOracle.Execute: IDBResultSet;
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

procedure TCatalogMetadataOracle.GetDatabaseMetadata;
begin
  inherited;
  GetCatalogs;
end;

procedure TCatalogMetadataOracle.GetCatalogs;
begin
  inherited;
  FCatalogMetadata.Name := '';
  GetSchemas;
end;

procedure TCatalogMetadataOracle.GetChecks(ATable: TTableMIK);
var
  oDBResultSet: IDBResultSet;
  oCheck: TCheckMIK;
begin
  inherited;
  FSQLText := GetSelectChecks(ATable.Name);
  oDBResultSet := Execute;
  while oDBResultSet.NotEof do
  begin
    oCheck := TCheckMIK.Create(ATable);
    oCheck.Name := VarToStr(oDBResultSet.GetFieldValue('check_name'));
    oCheck.Description := '';
    oCheck.Condition := VarToStr(oDBResultSet.GetFieldValue('check_condition'));
    ATable.Checks.Add(UpperCase(oCheck.Name), oCheck);
  end;
end;

procedure TCatalogMetadataOracle.GetSchemas;
begin
  inherited;
  FCatalogMetadata.Schema := '';
  GetSequences;
  GetTables;
end;

procedure TCatalogMetadataOracle.GetTables;
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

procedure TCatalogMetadataOracle.GetColumns(ATable: TTableMIK);

  function ExtractDefaultValue(ADefaultValue: Variant): string;
  begin
    Result := '';
    if ADefaultValue <> Null then
      Result := Trim(ADefaultValue);
    if Result = '0.000' then
      Result := '0';
  end;

  function ResolveIntegerNullValue(AValue: Variant): Integer;
  begin
    Result := 0;
    if AValue <> Null then
      Result := VarAsType(AValue, varInteger);
  end;

var
  oDBResultSet: IDBResultSet;
  oColumn: TColumnMIK;
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
    oColumn.NotNull := VarToStr(oDBResultSet.GetFieldValue('column_nullable')) = 'N';
    oColumn.DefaultValue := ExtractDefaultValue(VarToStr(oDBResultSet.GetFieldValue('column_defaultvalue')));
    oColumn.Description := VarToStr(oDBResultSet.GetFieldValue('column_description'));
    oColumn.TypeName := VarToStr(oDBResultSet.GetFieldValue('column_typename'));
    /// <summary>
    /// Seta o tipo do campo
    /// </summary>
    SetFieldType(oColumn);
    /// <summary>
    /// Resolve Field Type
    /// </summary>
    GetFieldTypeDefinition(oColumn);
    ATable.Fields.Add(FormatFloat('000000', oColumn.Position), oColumn);
  end;
end;

procedure TCatalogMetadataOracle.GetPrimaryKey(ATable: TTableMIK);

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

var
  oDBResultSet: IDBResultSet;
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

procedure TCatalogMetadataOracle.SetFieldType(var AColumnMIK: TColumnMIK);
begin
  if AColumnMIK.TypeName = 'NUMBER' then
  begin
    if AColumnMIK.Precision = 0 then
    begin
      /// (ftSmallint, ftInteger, ftWord, ftLargeint, ftAutoInc, ftLongWord, ftShortint, ftByte);
      AColumnMIK.FieldType := ftInteger;
      AColumnMIK.Size := 0;
    end
    else if AColumnMIK.Precision > 0 then
    begin
      /// (ftFloat, ftCurrency, ftBCD, ftExtended, ftSingle, ftFMTBcd);
      AColumnMIK.FieldType := ftCurrency;
    end;
    Exit
  end
  else
  if MatchText(AColumnMIK.TypeName, ['BLOB','CLOB','NCLOB']) then
    AColumnMIK.Size := 0;

  inherited SetFieldType(AColumnMIK);
end;

procedure TCatalogMetadataOracle.GetForeignKeys(ATable: TTableMIK);

  procedure GetForeignKeyColumns(AForeignKey: TForeignKeyMIK);
  var
    oDBResultSet: IDBResultSet;
    oFromField: TColumnMIK;
    oToField: TColumnMIK;
  begin
    FSQLText := GetSelectForeignKeyColumns(AForeignKey.Name);
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
begin
  inherited;
  FSQLText := GetSelectForeignKey(ATable.Name);
  oDBResultSet := Execute;
  while oDBResultSet.NotEof do
  begin
    oForeignKey := TForeignKeyMIK.Create(ATable);
    oForeignKey.Name := VarToStr(oDBResultSet.GetFieldValue('fk_name'));
    oForeignKey.FromTable := VarToStr(oDBResultSet.GetFieldValue('table_reference'));
    oForeignKey.OnUpdate := None;
    oForeignKey.OnDelete := GetRuleAction(VarToStr(oDBResultSet.GetFieldValue('fk_deleteaction')));
    oForeignKey.Description := VarToStr(oDBResultSet.GetFieldValue('fk_description'));
    ATable.ForeignKeys.Add(oForeignKey.Name, oForeignKey);
    /// <summary>
    /// Gera a lista de campos do foreignkey
    /// </summary>
    GetForeignKeyColumns(oForeignKey);
  end;
end;

procedure TCatalogMetadataOracle.GetFunctions;
begin
  inherited;

end;

procedure TCatalogMetadataOracle.GetProcedures;
begin
  inherited;

end;

procedure TCatalogMetadataOracle.GetSequences;
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

procedure TCatalogMetadataOracle.GetTriggers(ATable: TTableMIK);
var
  oDBResultSet: IDBResultSet;
  oTrigger: TTriggerMIK;
begin
  inherited;
  FSQLText := GetSelectTriggers(ATable.Name);
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

procedure TCatalogMetadataOracle.GetIndexeKeys(ATable: TTableMIK);

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

var
  oDBResultSet: IDBResultSet;
  oIndexeKey: TIndexeKeyMIK;
begin
  inherited;
  FSQLText := GetSelectIndexe(ATable.Name);
  oDBResultSet := Execute;
  while oDBResultSet.NotEof do
  begin
    oIndexeKey := TIndexeKeyMIK.Create(ATable);
    oIndexeKey.Name := VarToStr(oDBResultSet.GetFieldValue('indexe_name'));
    oIndexeKey.Unique := VarToStr(oDBResultSet.GetFieldValue('indexe_unique')) = 'UNIQUE';
    ATable.IndexeKeys.Add(UpperCase(oIndexeKey.Name), oIndexeKey);
    /// <summary>
    /// Gera a lista de campos do indexe
    /// </summary>
    GetIndexeKeyColumns(oIndexeKey);
  end;
end;

procedure TCatalogMetadataOracle.GetViews;
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

function TCatalogMetadataOracle.GetSelectPrimaryKey(ATableName: string): string;
begin
   Result := ' select rc.constraint_name as pk_name, ' +
             '        ''''               as pk_description ' +
             ' from user_constraints rc ' +
             ' where (rc.constraint_type = ''P'') ' +
             ' and   (rc.table_name = ' + QuotedStr(ATableName) + ')' +
             ' order by rc.constraint_name ';
end;

function TCatalogMetadataOracle.GetSelectPrimaryKeyColumns(APrimaryKeyName: string): string;
begin
   Result := ' select c.column_name as column_name, ' +
             '        c.position    as column_position ' +
             ' from user_constraints t ' +
             ' inner join user_cons_columns c on c.constraint_name = t.constraint_name ' +
             ' where (t.constraint_type = ''P'') ' +
             ' and t.table_name = ' + QuotedStr(APrimaryKeyName) +
             ' order by t.table_name, ' +
             '          t.constraint_name, ' +
             '          c.position ';
end;

function TCatalogMetadataOracle.GetSelectSequences: string;
begin
  Result := ' select sequence_name as name, ' +
            '        ''''          as description ' +
            ' from user_sequences ';
end;

function TCatalogMetadataOracle.GetSelectTableColumns(ATableName: string): string;
begin
   Result := ' select c.column_name        as column_name, ' +
             '        c.column_id          as column_position, ' +
             '        c.data_length        as column_size, ' +
             '        c.data_precision     as column_precision, ' +
             '        c.data_scale         as column_scale, ' +
             '        ''''                 as column_collation, ' +
             '        c.nullable           as column_nullable, ' +
             '        c.data_default       as column_defaultvalue, ' +
             '        a.comments           as column_description, ' +
             '        c.data_type          as column_typename, ' +
             '        c.character_set_name as column_charset ' +
             ' from user_tab_cols c' +
             ' inner join user_col_comments a on a.table_name = c.table_name ' +
             '                              and a.column_name = c.column_name ' +
             ' where c.table_name in (' + QuotedStr(ATableName) + ') ' +
             ' order by c.column_id';
end;

function TCatalogMetadataOracle.GetSelectTables: string;
begin
  Result := ' select tab.table_name as table_name, ' +
            '        com.comments   as table_description ' +
            ' from user_tables tab ' +
            ' inner join user_tab_comments com on com.table_name = tab.table_name ' +
            ' where tab.table_name not like ''%$%'' ' +
            ' order by tab.table_name ';
end;

function TCatalogMetadataOracle.GetSelectTriggers(ATableName: string): string;
begin
  { TODO -oISAQUE : Falta checar campos para preencher a classe }
  Result := ' select tg.trigger_name, ' +
            '        tg.table_name, ' +
            '        tg.trigger_type, ' +
            '        tg.triggering_event, ' +
            '        tg.base_object_type, ' +
            '        tg.column_name, ' +
            '        tg.referencing_names, ' +
            '        tg.when_clause, ' +
            '        tg.status, ' +
            '        tg.description, tg.trigger_body, ' +
            ' dbms_metadata.get_ddl(''TRIGGER'', tg.trigger_name) as ddl ' +
            ' from sys.user_triggers t ' +
            ' where tg.constraint_name not like ''%$%'' ' +
            ' and tg.constraint_name not like ''SYS_%'' ' +
            ' where tg.table_name = ' + QuotedStr(ATableName) +
            ' order by tg.trigger_name ';
end;

function TCatalogMetadataOracle.GetSelectChecks(ATableName: string): string;
begin
  Result := ' select ck.constraint_name  as check_name, ' +
            '        ck.search_condition as check_condition ' +
            ' from user_constraints ck ' +
            ' where ck.constraint_type = ''C'' ' +
            ' and ck.constraint_name not like ''%$%'' ' +
            ' and ck.constraint_name not like ''SYS_%'' ' +
            ' and ck.table_name = ' + QuotedStr(ATableName) +
            ' order by ck.constraint_name ';
end;

function TCatalogMetadataOracle.GetSelectForeignKey(ATableName: string): string;
begin
  Result := ' select cons.constraint_name as fk_name, ' +
            ' ''''                        as fk_updateaction, ' +
            ' cons.delete_rule            as fk_deleteaction, ' +
            ' conr.table_name             as table_reference, ' +
            ' cons.r_constraint_name      as fk_referencename, ' +
            ' ''''                        as fk_description ' +
            ' from user_constraints cons ' +
            ' left join user_constraints conr on conr.constraint_name = cons.r_constraint_name ' +
            ' where cons.table_name in(' + QuotedStr(ATableName) + ') ' +
            ' and cons.constraint_type in(''U'',''R'') ' +
            ' order by cons.constraint_name ';
end;

function TCatalogMetadataOracle.GetSelectForeignKeyColumns(AForeignKeyName: string): string;
begin
  Result := ' select cols.column_name as column_name, ' +
            '        cols.position    as column_position, ' +
            '        colr.column_name as column_reference, ' +
            '        colr.position    as column_referenceposition ' +
            ' from user_constraints cons ' +
            ' left join user_cons_columns cols on cols.constraint_name = cons.constraint_name ' +
            ' left join user_cons_columns colr on colr.constraint_name = cons.r_constraint_name ' +
            ' where cons.constraint_type = ''R'' and cols.position = colr.position ' +
            ' and cons.constraint_name in(' + QuotedStr(AForeignKeyName) +') ' +
            ' order by cols.position';
end;

function TCatalogMetadataOracle.GetSelectViews: string;
begin
  Result := ' select vw.view_name as view_name, ' +
            '        dbms_metadata.get_ddl(''VIEW'', vw.view_name) as view_script, ' +
            '        '''' as view_description, ' +
            ' from user_views vw';
end;

function TCatalogMetadataOracle.GetSelectIndexe(ATableName: string): string;
begin
  Result := ' select idx.index_name as indexe_name, ' +
            '        idx.uniqueness as indexe_unique, ' +
            '        ''''           as indexe_description ' +
            ' from user_indexes idx ' +
            ' inner join user_constraints usc on idx.table_owner = usc.owner(+) ' +
            '                               and idx.index_name = usc.index_name(+) ' +
            '                               and idx.table_name = usc.table_name(+) ' +
            ' where (usc.constraint_type not in(''P'') or usc.constraint_type is null) ' +
            ' and   idx.index_type not in(''LOB'') ' +
            ' and   idx.table_name in(' + QuotedStr(ATableName) + ') ' +
            ' order by idx.table_name, idx.index_name';
end;

function TCatalogMetadataOracle.GetSelectIndexeColumns(AIndexeName: string): string;
begin
  Result := ' select c.column_name, ' +
            '        c.column_position, ' +
            ' case when c.descend = ''DESC'' ' +
            '      then ''D'' ' +
            '      else ''A'' ' +
            ' end as sort_order, ' +
            ' e.column_expression, ' +
            ' case when e.column_expression is null ' +
            '      then 1 ' +
            '      else 0 ' +
            ' end as isnull_expression ' +
            ' from user_ind_columns c, user_ind_expressions e ' +
//            ' where c.index_owner = e.index_owner(+) ' +
            ' where c.index_name = e.index_name(+) ' +
            '   and c.column_position = e.column_position(+) ' +
            '   and c.index_name in(' + QuotedStr(AIndexeName) + ') ' +
            ' order by c.column_position';
end;

initialization
  TMetadataRegister.GetInstance.RegisterMetadata(dnOracle, TCatalogMetadataOracle.Create);

end.

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

unit ormbr.metadata.mssql;

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
  TCatalogMetadataMSSQL = class(TCatalogMetadataAbstract)
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

procedure TCatalogMetadataMSSQL.CreateFieldTypeList;
begin
  if Assigned(FFieldType) then
  begin
    FFieldType.Clear;
    FFieldType.Add('BIGINT', ftLargeint);
    FFieldType.Add('BINARY', ftSmallint);
    FFieldType.Add('CHAR', ftFixedChar);
    FFieldType.Add('CARACTER', ftFixedChar);
    FFieldType.Add('DATE', ftDate);
    FFieldType.Add('DATETIME2', ftDateTime);
    FFieldType.Add('DECIMAL', ftBCD);
    FFieldType.Add('DOUBLE PRECISION', ftExtended);
    FFieldType.Add('FLOAT', ftFloat);
    FFieldType.Add('INT', ftInteger);
    FFieldType.Add('INTEGER', ftInteger);
    FFieldType.Add('NCHAR', ftFixedWideChar);
    FFieldType.Add('NVARCHAR', ftWideString);
    FFieldType.Add('VARCHAR', ftString);
    FFieldType.Add('REAL', ftFloat);
    FFieldType.Add('MONEY', ftBCD);
    FFieldType.Add('NUMERIC', ftFloat);
    FFieldType.Add('VARCHAR(MAX)', ftWideMemo);
    FFieldType.Add('TEXT', ftMemo);
    FFieldType.Add('SMALLINT', ftSmallint);
    FFieldType.Add('TIME', ftTime);
    FFieldType.Add('GUID', ftGuid);
    FFieldType.Add('VARBINARY(MAX)', ftBlob);
  end;
end;

function TCatalogMetadataMSSQL.Execute: IDBResultSet;
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

procedure TCatalogMetadataMSSQL.GetDatabaseMetadata;
begin
  inherited;
  GetCatalogs;
end;

procedure TCatalogMetadataMSSQL.GetCatalogs;
begin
  inherited;
  FCatalogMetadata.Name := '';
  GetSchemas;
end;

procedure TCatalogMetadataMSSQL.GetChecks(ATable: TTableMIK);
begin
  /// Not Suported.
end;

procedure TCatalogMetadataMSSQL.GetSchemas;
begin
  inherited;
  FCatalogMetadata.Schema := '';
  GetSequences;
  GetTables;
end;

procedure TCatalogMetadataMSSQL.GetTables;
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

procedure TCatalogMetadataMSSQL.GetColumns(ATable: TTableMIK);
var
  oDBResultSet: IDBResultSet;
  oColumn: TColumnMIK;

  function ExtractDefaultValue(ADefaultValue: string): string;
  begin
     Result := ADefaultValue;
     Result := StringReplace(Result, '(', '', [rfReplaceAll]);
     Result := StringReplace(Result, ')', '', [rfReplaceAll]);
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
    oColumn.Size := VarAsType(oDBResultSet.GetFieldValue('column_size'), varInteger);
    oColumn.Precision := VarAsType(oDBResultSet.GetFieldValue('column_precision'), varInteger);
    oColumn.Scale := VarAsType(oDBResultSet.GetFieldValue('column_scale'), varInteger);
    oColumn.NotNull := not VarAsType(oDBResultSet.GetFieldValue('column_nullable'), varBoolean);
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

procedure TCatalogMetadataMSSQL.GetPrimaryKey(ATable: TTableMIK);
var
  oDBResultSet: IDBResultSet;

  procedure GetPrimaryKeyColumns(APrimaryKey: TPrimaryKeyMIK);
  var
    oDBResultSet: IDBResultSet;
    oColumn: TColumnMIK;
  begin
    FSQLText := GetSelectPrimaryKeyColumns(APrimaryKey.Name);
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

procedure TCatalogMetadataMSSQL.GetForeignKeys(ATable: TTableMIK);

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

procedure TCatalogMetadataMSSQL.GetFunctions;
begin
  inherited;

end;

procedure TCatalogMetadataMSSQL.GetProcedures;
begin
  inherited;

end;

procedure TCatalogMetadataMSSQL.GetSequences;
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
    oSequence.Name := VarToStr(oDBResultSet.GetFieldValue('name'));
    oSequence.Description := VarToStr(oDBResultSet.GetFieldValue('description'));;
    FCatalogMetadata.Sequences.Add(UpperCase(oSequence.Name), oSequence);
  end;
end;

procedure TCatalogMetadataMSSQL.GetTriggers(ATable: TTableMIK);
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

procedure TCatalogMetadataMSSQL.GetIndexeKeys(ATable: TTableMIK);
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
    oIndexeKey.Unique := VarAsType(oDBResultSet.GetFieldValue('indexe_unique'), varBoolean);
    ATable.IndexeKeys.Add(UpperCase(oIndexeKey.Name), oIndexeKey);
    /// <summary>
    /// Gera a lista de campos do indexe
    /// </summary>
    GetIndexeKeyColumns(oIndexeKey);
  end;
end;

procedure TCatalogMetadataMSSQL.GetViews;
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

function TCatalogMetadataMSSQL.GetSelectPrimaryKey(ATableName: string): string;
begin
  Result := ' select si.name as pk_name, ' +
            '       (select value from [fn_listextendedproperty] (''MS_Description'', ''schema'', ss.name, ''table'', tb.name, null, null)) as pk_description ' +
            ' from sys.indexes si ' +
            ' inner join sys.objects tb on tb.object_id = si.object_id ' +
            ' inner join sys.schemas ss on ss.schema_id = tb.schema_id ' +
            ' where si.is_primary_key = 1 and tb.name = ' + QuotedStr(ATableName) +
            ' order by si.name';
end;

function TCatalogMetadataMSSQL.GetSelectPrimaryKeyColumns(APrimaryKeyName: string): string;
begin
  Result := ' select si.name              as pk_name, ' +
            '        ic.key_ordinal       as column_position, ' +
            '        sc.name              as column_name, ' +
            '        ic.is_descending_key as index_type ' +
            ' from sys.index_columns ic ' +
            ' inner join sys.indexes si on ic.object_id = si.object_id and ic.index_id = si.index_id ' +
            ' inner join sys.columns sc on ic.object_id = sc.object_id and ic.column_id = sc.column_id  ' +
            ' where si.is_primary_key = 1 and si.name = ' + QuotedStr(APrimaryKeyName) +
            ' order by si.name, ic.key_ordinal';
end;

function TCatalogMetadataMSSQL.GetSelectSequences: string;
begin
  Result := ' select name   as name, ' +
            '      ''desc'' as description' +
            ' from [sys].sequences';
end;

function TCatalogMetadataMSSQL.GetSelectTableColumns(ATableName: string): string;
begin
  Result := ' select ac.name           as column_name, ' +
            '        ac.column_id      as column_position, ' +
            '        ac.max_length     as column_size, ' +
            '        ac.precision      as column_precision, ' +
            '        ac.scale          as column_scale, ' +
            '        ac.collation_name as column_collation, ' +
            '        ac.is_nullable    as column_nullable, ' +
            '        dc.definition     as column_defaultvalue, ' +
            '        ep.value          as column_description, ' +
            '        ob.name           as domain_name, ' +
            '        upper(tp.name)    as column_typename ' +
            ' from sys.all_columns ac ' +
            ' inner join sys.all_objects ao on ao.object_id = ac.object_id ' +
            ' inner join sys.types tp on ac.user_type_id = tp.user_type_id ' +
            ' left outer join sys.objects ob on ob.object_id = ac.default_object_id ' +
            ' left outer join sys.default_constraints dc on dc.object_id = ob.object_id ' +
            ' left outer join sys.extended_properties ep on ep.class = 1 ' +
            '                                            and ep.major_id = ac.object_id ' +
            '                                            and ep.minor_id = ac.column_id ' +
            '                                            and ep.name = ''MS_Description'' ' +
            ' where ao.name in(' + quotedstr(ATableName) + ') ' +
            ' order by ac.column_id';
end;

function TCatalogMetadataMSSQL.GetSelectTables: string;
begin
  Result := ' select ss.name as table_schema, ' +
            '        so.name as table_name, ' +
            '       (select value from [fn_listextendedproperty] (''MS_Description'', ''schema'', ss.name, ''table'', so.name, null, null)) as table_description ' +
            ' from sys.objects so ' +
            ' left join sys.schemas ss on ss.schema_id = so.schema_id ' +
            ' where so.type = ''U'' and so.is_ms_shipped = 0 ' +
            ' order by ss.name, so.name';
end;

function TCatalogMetadataMSSQL.GetSelectTriggers(ATableName: string): string;
begin
  Result := '';
end;

function TCatalogMetadataMSSQL.GetSelectForeignKey(ATableName: string): string;
begin
  Result := ' select fk.name                      as fk_name, ' +
            '        fk.update_referential_action as fk_updateaction, ' +
            '        fk.delete_referential_action as fk_deleteaction, ' +
            '        sfc.name                     as column_name, ' +
            '        ft.name                      as table_reference, ' +
            '        src.name                     as column_reference, ' +
            '        fkc.constraint_column_id     as column_position, ' +
            '        ep.value                     as fk_description ' +
            ' from sys.foreign_key_columns fkc ' +
            ' inner join sys.columns sfc on sfc.object_id = fkc.parent_object_id and sfc.column_id = fkc.parent_column_id ' +
            ' inner join sys.columns src on src.object_id = fkc.referenced_object_id and src.column_id = fkc.referenced_column_id ' +
            ' inner join sys.foreign_keys fk on fk.object_id = fkc.constraint_object_id ' +
            ' inner join sys.objects ft on ft.object_id = fk.parent_object_id and ft.type = ''U'' ' +
            ' left outer join sys.extended_properties ep on ep.major_id = fk.object_id ' +
            '                                           and ep.name =     ''MS_Description'' ' +
            ' where coalesce(fk.is_ms_shipped, 0) <> 1 and ft.name in(' + QuotedStr(ATableName) + ') ' +
            ' order by ft.name, fkc.constraint_column_id';
end;

function TCatalogMetadataMSSQL.GetSelectForeignKeyColumns(AForeignKeyName: string): string;
begin
  Result := 'Falta implementar';
end;

function TCatalogMetadataMSSQL.GetSelectViews: string;
begin
   Result := ' select iv.table_name   as view_name, ' +
             '        sm.definition   as view_script, ' +
             '        ep.value        as view_description, ' +
             '        sm.uses_ansi_nulls        as view_usesansinulls, ' +
             '        sm.uses_quoted_identifier as view_usesquotedidentifier ' +
             ' from sys.views sv ' +
             ' left join information_schema.views iv on iv.table_name = sv.name ' +
             ' left join sys.sql_modules sm on sm.object_id = sv.object_id ' +
             ' left join sys.extended_properties ep on ep.major_id = sv.object_id ' +
             '                                     and ep.minor_id = 0 ' +
             '                                     and ep.name = ''ms_description'' ';
end;

function TCatalogMetadataMSSQL.GetSelectIndexe(ATableName: string): string;
begin
   Result := ' select si.name              as indexe_name, ' +
             '        si.name              as sysindexe_name, ' +
             '        si.is_unique         as indexe_unique, ' +
             '  (select value from [fn_listextendedproperty] (''MS_Description'', ''schema'', ss.name, ''table'', tb.name, null, null)) as indexe_description ' +
             ' from sys.indexes si ' +
             ' inner join sys.objects tb on tb.object_id = si.object_id ' +
             ' inner join sys.schemas ss on ss.schema_id = tb.schema_id ' +
             ' where si.type = 2 and tb.name = ' + QuotedStr(ATableName) +
             ' order by tb.name';
end;

function TCatalogMetadataMSSQL.GetSelectIndexeColumns(AIndexeName: string): string;
begin
   Result := ' select sc.name              as column_name, ' +
             '        ic.key_ordinal       as column_position, ' +
             '        ic.is_descending_key as index_type ' +
             ' from sys.index_columns ic ' +
             ' inner join sys.indexes si on ic.object_id = si.object_id and ic.index_id = si.index_id ' +
             ' inner join sys.columns sc on ic.object_id = sc.object_id and ic.column_id = sc.column_id  ' +
             ' where si.type = 2 and si.name = ' + QuotedStr(AIndexeName) +
             ' order by si.name, ' +
             '          ic.key_ordinal';
end;

initialization
  TMetadataRegister.GetInstance.RegisterMetadata(dnMSSQL, TCatalogMetadataMSSQL.Create);

end.

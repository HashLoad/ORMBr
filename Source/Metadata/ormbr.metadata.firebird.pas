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

  @abstract(Contribuição - Carlos Eduardo R. Grillo)
}

unit ormbr.metadata.firebird;

interface

uses
  SysUtils,
  Variants,
  DB,
  ormbr.metadata.register,
  ormbr.metadata.extract,
  ormbr.database.mapping,
  ormbr.factory.interfaces,
  ormbr.types.mapping,
  ormbr.types.database;

type
  TCatalogMetadataFirebird = class(TCatalogMetadataAbstract)
  private
    procedure ResolveFieldType(AColumn: TColumnMIK; AColumnType: Integer; AColumnSubType: Variant);
    function GetSelectViewsColumns(AViewName: string): string;
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

{ TSchemaExtractFirebird }

function TCatalogMetadataFirebird.Execute: IDBResultSet;
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

procedure TCatalogMetadataFirebird.GetDatabaseMetadata;
begin
  inherited;
  GetCatalogs;
end;

procedure TCatalogMetadataFirebird.GetCatalogs;
begin
  inherited;
  FCatalogMetadata.Name := '';
  GetSchemas;
end;

procedure TCatalogMetadataFirebird.GetChecks(ATable: TTableMIK);
begin
  /// Not Suported.
end;

procedure TCatalogMetadataFirebird.GetSchemas;
begin
  inherited;
  FCatalogMetadata.Schema := '';
  GetSequences;
  GetTables;
  GetViews;
end;

procedure TCatalogMetadataFirebird.GetTables;
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

procedure TCatalogMetadataFirebird.GetColumns(ATable: TTableMIK);
var
  oDBResultSet: IDBResultSet;
  oColumn: TColumnMIK;

  function ExtractDefaultValue(ADefaultValue: string): string;
  var
    iDefaultPos: Integer;
  begin
    Result := UpperCase(Trim(ADefaultValue));
    if Length(ADefaultValue) > 0 then
    begin
      iDefaultPos := Pos('DEFAULT', Result);
      if iDefaultPos = 1 then
        Delete(Result,1,7);
      iDefaultPos := Pos('=', Result);
      if iDefaultPos = 1 then
        Delete(Result,1,1);
    end;
    Result := Trim(Result);
  end;

  function ResolveIntegerNullValue(AValue: Variant): Integer;
  begin
    Result := 0;
    if AValue <> Null then
      Result := VarAsType(AValue, varInteger);
  end;

  function ResolveBooleanNullValue(AValue: Variant): Boolean;
  begin
    Result := False;
    if AValue <> Null then
      Result := VarAsType(AValue, varBoolean);
  end;

begin
  inherited;
  FSQLText := GetSelectTableColumns(ATable.Name);
  oDBResultSet := Execute;
  while oDBResultSet.NotEof do
  begin
    oColumn := TColumnMIK.Create(ATable);
    oColumn.Name := VarToStr(oDBResultSet.GetFieldValue('column_name'));
    oColumn.Position := VarAsType(oDBResultSet.GetFieldValue('column_position'), varInteger);
    oColumn.Size := ResolveIntegerNullValue(oDBResultSet.GetFieldValue('column_size'));
    oColumn.Precision := ResolveIntegerNullValue(oDBResultSet.GetFieldValue('column_precision'));
    oColumn.Scale := ResolveIntegerNullValue(oDBResultSet.GetFieldValue('column_scale'));
    oColumn.Scale := (oColumn.Scale) * (-1);
    oColumn.NotNull := ResolveBooleanNullValue(oDBResultSet.GetFieldValue('column_nullable'));
    oColumn.DefaultValue := ExtractDefaultValue(VarToStr(oDBResultSet.GetFieldValue('column_defaultvalue')));
    oColumn.Description := VarToStr(oDBResultSet.GetFieldValue('column_description'));
    oColumn.CharSet := VarToStr(oDBResultSet.GetFieldValue('column_charset'));
    ResolveFieldType(oColumn,
                     VarAsType(oDBResultSet.GetFieldValue('column_type'), varInteger),
                     ResolveIntegerNullValue(oDBResultSet.GetFieldValue('column_subtype')));
    /// <summary>
    /// Resolve Field Type
    /// </summary>
    GetFieldTypeDefinition(oColumn);
    ATable.Fields.Add(FormatFloat('000000', oColumn.Position), oColumn);
  end;
end;

procedure TCatalogMetadataFirebird.GetPrimaryKey(ATable: TTableMIK);
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
      oColumn.Position := VarAsType(oDBResultSet.GetFieldValue('column_position'), varInteger);
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

procedure TCatalogMetadataFirebird.GetForeignKeys(ATable: TTableMIK);

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
      oFromField.Position := VarAsType(oDBResultSet.GetFieldValue('column_position'), varInteger);
      AForeignKey.FromFields.Add(FormatFloat('000000', oFromField.Position), oFromField);
      /// <summary>
      /// Coluna tabela referencia
      /// </summary>
      oToField := TColumnMIK.Create(ATable);
      oToField.Name := VarToStr(oDBResultSet.GetFieldValue('column_reference'));
      oToField.Position := VarAsType(oDBResultSet.GetFieldValue('column_referenceposition'), varInteger);
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
    oForeignKey.OnUpdate := GetRuleAction(VarToStrDef(oDBResultSet.GetFieldValue('fk_updateaction'), 'RESTRICT'));
    oForeignKey.OnDelete := GetRuleAction(VarToStrDef(oDBResultSet.GetFieldValue('fk_deleteaction'), 'RESTRICT'));
    oForeignKey.Description :=  VarToStr(oDBResultSet.GetFieldValue('fk_description'));
    ATable.ForeignKeys.Add(UpperCase(oForeignKey.Name), oForeignKey);
    /// <summary>
    /// Gera a lista de campos do foreignkey
    /// </summary>
    GetForeignKeyColumns(oForeignKey);
  end;
end;

procedure TCatalogMetadataFirebird.GetFunctions;
begin
  inherited;

end;

procedure TCatalogMetadataFirebird.GetProcedures;
begin
  inherited;

end;

procedure TCatalogMetadataFirebird.GetSequences;
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
    oSequence.Description := VarToStr(oDBResultSet.GetFieldValue('description'));
    FCatalogMetadata.Sequences.Add(UpperCase(oSequence.Name), oSequence);
  end;
end;

procedure TCatalogMetadataFirebird.GetTriggers(ATable: TTableMIK);
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

procedure TCatalogMetadataFirebird.GetIndexeKeys(ATable: TTableMIK);
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
      oColumn.Position := VarAsType(oDBResultSet.GetFieldValue('column_position'), varInteger);
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

procedure TCatalogMetadataFirebird.GetViews;
var
  oDBResultSet: IDBResultSet;
  oView: TViewMIK;

  procedure GetViewColumns(AView: TViewMIK);
  var
    oDBResultSet: IDBResultSet;
    oColumn: TColumnMIK;
  begin
    inherited;
    FSQLText := GetSelectViewsColumns(AView.Name);
    oDBResultSet := Execute;
    while oDBResultSet.NotEof do
    begin
      oColumn := TColumnMIK.Create;
      oColumn.Name := VarToStr(oDBResultSet.GetFieldValue('column_name'));
      AView.Fields.Add(UpperCase(oColumn.Name), oColumn);
    end;
  end;

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
    /// <summary>
    /// Gera a lista de campos da view
    /// </summary>
    GetViewColumns(oView);
  end;
end;

procedure TCatalogMetadataFirebird.ResolveFieldType(AColumn: TColumnMIK;
  AColumnType: Integer; AColumnSubType: Variant);
begin
  case AColumnType of
     7 : AColumn.FieldType := ftSmallint;
     8 : AColumn.FieldType := ftInteger;
//     9 : AColumn.TypeName := 'QUAD';
    10 : AColumn.FieldType := ftFloat;
    12 : AColumn.FieldType := ftDate;
    13 : AColumn.FieldType := ftTime;
    14 : AColumn.FieldType := ftFixedChar;
    16 : AColumn.FieldType := ftLargeint;
    17 : AColumn.FieldType := ftBoolean;
    23 : AColumn.FieldType := ftBoolean;
    27 : AColumn.FieldType := ftExtended;
    35 : AColumn.FieldType := ftTimeStamp;
//    35 : begin // Dialect=1: Date - Dialect=3: TimeStamp
//         if ADialect = 1 then
//           AColumn.FieldType := ftDate;
//         else
//           AColumn.FieldType := ftTimeStamp;
//         end;
    37 : AColumn.FieldType := ftString;
    40 : AColumn.FieldType := ftWideString;
    261: AColumn.FieldType := ftBlob;
  end;
  if AColumnType = 14 then
  begin
    case AColumnSubType of
      3: AColumn.FieldType := ftFixedChar;
    else
      AColumn.FieldType := ftFixedChar;
    end;
  end;
  if AColumnType = 16 then
  begin
    case AColumnSubType of
      0: AColumn.FieldType := ftLargeint;
      1: AColumn.FieldType := ftCurrency;
      2: AColumn.FieldType := ftBCD;
    else
      AColumn.FieldType := ftLargeint;
    end;
  end;
  if AColumnType = 261 then
  begin
    case AColumnSubType of
      0: AColumn.FieldType := ftBlob;
      1: AColumn.FieldType := ftMemo;
    else
      AColumn.FieldType := ftMemo;
    end;
  end;
end;

function TCatalogMetadataFirebird.GetSelectPrimaryKey(ATableName: string): string;
begin
   Result := ' select rc.rdb$constraint_name as pk_name, ' +
             '        id.rdb$description     as pk_description ' +
             ' from rdb$relation_constraints rc, rdb$indices id ' +
             ' where (id.rdb$index_name = rc.rdb$index_name) ' +
             ' and   (rc.rdb$constraint_type = ''PRIMARY KEY'') ' +
             ' and   (rc.rdb$relation_name = ' + QuotedStr(ATableName) + ')' +
             ' order by rc.rdb$relation_name ';
end;

function TCatalogMetadataFirebird.GetSelectPrimaryKeyColumns(APrimaryKeyName: string): string;
begin
   Result := ' select rc.rdb$constraint_name as pk_name, ' +
             '        ix.rdb$field_position  as column_position, ' +
             '        ix.rdb$field_name      as column_name ' +
             ' from rdb$relation_constraints rc, rdb$index_segments ix ' +
             ' where (ix.rdb$index_name = rc.rdb$index_name) ' +
             ' and   (rc.rdb$constraint_type = ''PRIMARY KEY'') ' +
             ' and   (rc.rdb$constraint_name = ' + QuotedStr(APrimaryKeyName) + ')' +
             ' order by rc.rdb$relation_name, ' +
             '          ix.rdb$field_position';
end;

function TCatalogMetadataFirebird.GetSelectSequences: string;
begin
  Result := ' select rdb$generator_name as name, ' +
            '        rdb$description    as description     ' +
            ' from rdb$generators ' +
            ' where (rdb$system_flag <> 1 or rdb$system_flag is null) ' +
//            ' and rdb$generator_name not like ''IBE$%'' ' +
            ' order by rdb$generator_name ';
end;

function TCatalogMetadataFirebird.GetSelectTableColumns(ATableName: string): string;
begin
   Result := ' select rf.rdb$field_name           as column_name, ' +
             '        rf.rdb$field_position       as column_position, ' +
             '        fs.rdb$character_length     as column_size, ' +
             '        fs.rdb$field_precision      as column_precision, ' +
             '        fs.rdb$field_scale          as column_scale, ' +
             '        co.rdb$collation_name       as column_collation, ' +
             '        rf.rdb$null_flag            as column_nullable, ' +
             '        rf.rdb$default_source       as column_defaultvalue, ' +
             '        rf.rdb$description          as column_description, ' +
             '        fs.rdb$field_name           as domain_name, ' +
             '        fs.rdb$field_type           as column_type, ' +
             '        fs.rdb$field_sub_type       as column_subtype, ' +
             '        cs.rdb$character_set_name   as column_charset ' +
             ' from rdb$relations rl ' +
             ' left join rdb$relation_fields rf on rf.rdb$relation_name = rl.rdb$relation_name ' +
             ' left join rdb$fields fs on fs.rdb$field_name = rf.rdb$field_source ' +
             ' left join rdb$character_sets cs on fs.rdb$character_set_id = cs.rdb$character_set_id ' +
             ' left join rdb$collations co on ((rf.rdb$collation_id = co.rdb$collation_id) and (fs.rdb$character_set_id = co.rdb$character_set_id)) ' +
             ' where rl.rdb$relation_name in (' + QuotedStr(ATableName) + ') ' +
             ' order by rf.rdb$field_position' ;
end;

function TCatalogMetadataFirebird.GetSelectTables: string;
begin
  Result := ' select rl.rdb$owner_name    as table_schema, ' +
            '        rl.rdb$relation_name as table_name, ' +
            '        rl.rdb$description   as table_description ' +
            ' from rdb$relations rl' +
            ' where (rdb$view_blr is null) and (rdb$system_flag = 0) ';
end;

function TCatalogMetadataFirebird.GetSelectTriggers(ATableName: string): string;
begin
  Result := '';
end;

function TCatalogMetadataFirebird.GetSelectForeignKey(ATableName: string): string;
begin
  Result := ' select a.rdb$constraint_name as fk_name, ' +
            '        b.rdb$update_rule     as fk_updateaction, ' +
            '        b.rdb$delete_rule     as fk_deleteaction, ' +
            '        c.rdb$relation_name   as table_reference, ' +
            '        i.rdb$description     as fk_description ' +
            ' from rdb$ref_constraints b ' +
            ' inner join rdb$relation_constraints a on a.rdb$constraint_name = b.rdb$constraint_name and a.rdb$constraint_type = ''FOREIGN KEY'' ' +
            ' inner join rdb$relation_constraints c on b.rdb$const_name_uq = c.rdb$constraint_name ' +
            ' inner join rdb$indices i on i.rdb$index_name = a.rdb$index_name ' +
            ' where a.rdb$relation_name in(' + QuotedStr(ATableName) + ') ' +
            ' order by a.rdb$relation_name; ';
end;

function TCatalogMetadataFirebird.GetSelectForeignKeyColumns(AForeignKeyName: string): string;
begin
  Result := ' select idx.rdb$relation_name     as table_name, ' +
            '        idxs.rdb$field_name       as column_name, ' +
            '        rc.rdb$deferrable         as is_deferrable, ' +
            '        rc.rdb$initially_deferred as is_deferred, ' +
            '        refc.rdb$update_rule      as on_update, ' +
            '        refc.rdb$delete_rule      as on_delete, ' +
            '        refc.rdb$match_option     as match_type, ' +
            '        i2.rdb$relation_name      as references_table, ' +
            '        s2.rdb$field_name         as column_reference, ' +
            '        idx.rdb$description       as description, ' +
            '        idxs.rdb$field_position   as column_position, ' +
            '        s2.rdb$field_position     as column_referenceposition ' +
            ' from rdb$index_segments idxs ' +
            ' left join rdb$indices idx on idx.rdb$index_name = idxs.rdb$index_name ' +
            ' left join rdb$relation_constraints rc on rc.rdb$index_name = idxs.rdb$index_name ' +
            ' left join rdb$ref_constraints refc on rc.rdb$constraint_name = refc.rdb$constraint_name '+
            ' left join rdb$relation_constraints rc2 on rc2.rdb$constraint_name = refc.rdb$const_name_uq ' +
            ' left join rdb$indices i2 on i2.rdb$index_name = rc2.rdb$index_name ' +
            ' left join rdb$index_segments s2 on i2.rdb$index_name = s2.rdb$index_name ' +
            ' where rc.rdb$constraint_type = ''FOREIGN KEY'' ' +
            ' and rc.rdb$constraint_name in(' + QuotedStr(AForeignKeyName) + ') ' +
            ' order by idxs.rdb$field_position ';
end;

function TCatalogMetadataFirebird.GetSelectViews: string;
begin
   Result := ' select rl.rdb$relation_name as view_name, ' +
             '        rl.rdb$view_source   as view_script, ' +
             '        rl.rdb$description   as view_description' +
             ' from rdb$relations rl ' +
             ' where (rl.rdb$system_flag <> 1 or rl.rdb$system_flag is null) ' +
             ' and not (rl.rdb$view_blr is null) and (rl.rdb$flags = 1) ';
end;

function TCatalogMetadataFirebird.GetSelectViewsColumns(AViewName: String): string;
begin
  Result := ' select ' +
            '        rf.rdb$field_name           as column_name ' +
            ' from rdb$relations rl ' +
            ' left join rdb$relation_fields rf on rf.rdb$relation_name = rl.rdb$relation_name ' +
            ' where rl.rdb$relation_name in (' + QuotedStr(AViewName) + ') ';
end;
function TCatalogMetadataFirebird.GetSelectIndexe(ATableName: string): string;
begin
  Result := ' select   idx.rdb$index_name      as indexe_name, ' +
            ' coalesce(idx.rdb$unique_flag, 0) as indexe_unique, ' +
            '          idx.rdb$description     as indexe_description ' +
            ' from rdb$indices idx ' +
            ' left join rdb$relation_constraints rct on idx.rdb$index_name = rct.rdb$index_name ' +
            ' where idx.rdb$relation_name in(' + QuotedStr(ATableName) + ') ' +
            ' and  (rct.rdb$constraint_type in(''UNIQUE'') or rct.rdb$constraint_type is null) ' +
            ' order by idx.rdb$relation_name, idx.rdb$index_name';
end;

function TCatalogMetadataFirebird.GetSelectIndexeColumns(AIndexeName: string): string;
begin
   Result := ' select isg.rdb$field_name     as column_name, ' +
             '        isg.rdb$field_position as column_position, ' +
             '        idx.rdb$description    as column_description ' +
             ' from rdb$indices idx ' +
             ' left join rdb$index_segments isg on (isg.rdb$index_name = idx.rdb$index_name) ' +
             ' left join rdb$relation_constraints rct on (idx.rdb$index_name = rct.rdb$index_name) ' +
             ' where rct.rdb$constraint_type is null ' +
             ' and   idx.rdb$index_name in(' + QuotedStr(AIndexeName) + ') ' +
             ' order by idx.rdb$index_name, ' +
             '          isg.rdb$field_position desc';
end;

initialization
  TMetadataRegister.GetInstance.RegisterMetadata(dnFirebird, TCatalogMetadataFirebird.Create);

end.

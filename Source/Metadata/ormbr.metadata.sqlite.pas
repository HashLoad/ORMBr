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

unit ormbr.metadata.sqlite;

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
  TCatalogMetadataSQLite = class(TCatalogMetadataAbstract)
  private
    procedure ResolveFieldType(AColumn: TColumnMIK; ATypeName: string);
  protected
    function GetSelectTables: string; override;
    function GetSelectTableColumns(ATableName: string): string; override;
    function GetSelectPrimaryKey(ATableName: string): string; override;
    function GetSelectForeignKey(ATableName: string): string; override;
    function GetSelectIndexe(ATableName: string): string; override;
    function GetSelectIndexeColumns(AIndexeName: string): string; override;
    function GetSelectTriggers(ATableName: string): string; override;
    function GetSelectViews: string;
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

procedure TCatalogMetadataSQLite.CreateFieldTypeList;
begin
  if Assigned(FFieldType) then
  begin
    FFieldType.Clear;
    FFieldType.Add('INTEGER', ftInteger);
    FFieldType.Add('DATE', ftDate);
    FFieldType.Add('INT', ftInteger);
    FFieldType.Add('BIGINT', ftLargeint);
    FFieldType.Add('SMALLINT', ftSmallint);
    FFieldType.Add('CHAR', ftFixedChar);
    FFieldType.Add('VARCHAR', ftString);
    FFieldType.Add('NVARCHAR', ftWideString);
    FFieldType.Add('CLOB', ftMemo);
    FFieldType.Add('BLOB', ftMemo);
    FFieldType.Add('TEXT', ftMemo);
    FFieldType.Add('FLOAT', ftFloat);
    FFieldType.Add('REAL', ftFloat);
    FFieldType.Add('BOOLEAN', ftBoolean);
    FFieldType.Add('TIME', ftTime);
    FFieldType.Add('DATETIME', ftDateTime);
    FFieldType.Add('TIMESTAMP', ftTimeStamp);
    FFieldType.Add('NUMERIC', ftBCD);
    FFieldType.Add('DECIMAL', ftBCD);
    FFieldType.Add('GUID', ftGuid);
  end;
end;

function TCatalogMetadataSQLite.Execute: IDBResultSet;
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

procedure TCatalogMetadataSQLite.GetDatabaseMetadata;
begin
  inherited;
  GetCatalogs;
end;

procedure TCatalogMetadataSQLite.GetCatalogs;
begin
  inherited;
  FCatalogMetadata.Name := '';
  GetSchemas;
end;

procedure TCatalogMetadataSQLite.GetChecks(ATable: TTableMIK);
begin
  /// Not Suported.
end;

procedure TCatalogMetadataSQLite.GetSchemas;
begin
  inherited;
  FCatalogMetadata.Schema := '';
  GetTables;
end;

procedure TCatalogMetadataSQLite.GetTables;
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
    oTable.Name := VarToStr(oDBResultSet.GetFieldValue('name'));
    oTable.Description := '';
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

procedure TCatalogMetadataSQLite.GetColumns(ATable: TTableMIK);
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
    oColumn.Name := VarToStr(oDBResultSet.GetFieldValue('name'));
    oColumn.Description := oColumn.Name;
    oColumn.Position := VarAsType(oDBResultSet.GetFieldValue('cid'), varInteger);
    /// <summary>
    /// O método ResolveTypeField() extrai e popula as propriedades relacionadas abaixo
    /// </summary>
    /// <param name="AColumn: TColumnMIK">Informar [oColumn] para receber informações adicionais
    /// </param>
    /// <param name="ATypeName: String">Campo com descriçao do tipo que veio na extração do metadata
    /// </param>
    /// <remarks>
    /// Relação das propriedades que serão alimentadas no método ResolveTypeField()
    /// oColumn.TypeName: string;
    /// oColumn.FieldType: TTypeField;
    /// oColumn.Size: Integer;
    /// oColumn.Precision: Integer;
    /// oColumn.Scale: Integer;
    /// </remarks>
    ResolveFieldType(oColumn, VarToStr(oDBResultSet.GetFieldValue('type')));
    ///
    oColumn.NotNull := oDBResultSet.GetFieldValue('notnull') = 1;
    oColumn.DefaultValue := VarToStr(oDBResultSet.GetFieldValue('dflt_value'));
    ATable.Fields.Add(FormatFloat('000000', oColumn.Position), oColumn);
  end;
end;

procedure TCatalogMetadataSQLite.GetPrimaryKey(ATable: TTableMIK);

  function GetPrimaryKeyName(ATableName: string): string;
  var
    oDBResultSet: IDBResultSet;
  begin
    FSQLText := Format('PRAGMA index_list("%s")', [ATable.Name]);
    oDBResultSet := Execute;
    while oDBResultSet.NotEof do
    begin
      if VarToStr(oDBResultSet.GetFieldValue('origin')) = 'pk' then
        Exit('PK_' + ATableName);
    end;
  end;

  function GetColumnAutoIncrement(ATableName: string): Integer;
  var
    oDBResultSet: IDBResultSet;
  begin
    FSQLText := ' select count(*) as autoinc ' +
                ' from sqlite_sequence ' +
                ' where name = ''' + ATableName + ''' ' +
                ' order by name';
    try
      oDBResultSet := Execute;
    except
      Exit(0);
    end;
    Exit(oDBResultSet.GetFieldValue('autoinc'));
  end;

  procedure GetPrimaryKeyColumns(APrimaryKey: TPrimaryKeyMIK);
  var
    oDBResultSet: IDBResultSet;
    oColumn: TColumnMIK;
  begin
    FSQLText := Format('PRAGMA table_info("%s")', [ATable.Name]);
    oDBResultSet := Execute;
    while oDBResultSet.NotEof do
    begin
      oColumn := TColumnMIK.Create(ATable);
      oColumn.Name := VarToStr(oDBResultSet.GetFieldValue('name'));
      oColumn.NotNull := oDBResultSet.GetFieldValue('notnull') = 1;
      oColumn.Position := VarAsType(oDBResultSet.GetFieldValue('cid'), varInteger);
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
    if VarAsType(oDBResultSet.GetFieldValue('pk'), varInteger) = 1 then
    begin
      ATable.PrimaryKey.Name := GetPrimaryKeyName(ATable.Name);
      ATable.PrimaryKey.Description := '';
      ATable.PrimaryKey.AutoIncrement := GetColumnAutoIncrement(ATable.Name) > 0;
      /// <summary>
      /// Estrai as columnas da primary key
      /// </summary>
      GetPrimaryKeyColumns(ATable.PrimaryKey);
      Break
    end;
  end;
end;

procedure TCatalogMetadataSQLite.GetForeignKeys(ATable: TTableMIK);
var
  oDBResultSet: IDBResultSet;
  oForeignKey: TForeignKeyMIK;
  oFromField: TColumnMIK;
  oToField: TColumnMIK;
  iID: Integer;
begin
  inherited;
  iID := -1;
  /// <summary>
  /// No FireDAC ao executar o comando de extração das FKs de um table, e essa
  /// table não tiver FKs, ocorre um erro de Not ResultSet, mas se tiver, tudo
  /// ocorre normalmente, por isso o tratamento com try except
  /// </summary>
  try
    FSQLText := GetSelectForeignKey(ATable.Name);
    oDBResultSet := Execute;
    while oDBResultSet.NotEof do
    begin
      if iID <> VarAsType(oDBResultSet.GetFieldValue('id'), varInteger) then
      begin
        oForeignKey := TForeignKeyMIK.Create(ATable);
        oForeignKey.Name := Format('FK_%s_%s', [VarToStr(oDBResultSet.GetFieldValue('table')),
                                                VarToStr(oDBResultSet.GetFieldValue('from'))]);
        oForeignKey.FromTable := VarToStr(oDBResultSet.GetFieldValue('table'));
        oForeignKey.OnUpdate := GetRuleAction(VarToStr(oDBResultSet.GetFieldValue('on_update')));
        oForeignKey.OnDelete := GetRuleAction(VarToStr(oDBResultSet.GetFieldValue('on_delete')));
        iID := VarAsType(oDBResultSet.GetFieldValue('id'), varInteger);
        ATable.ForeignKeys.Add(oForeignKey.Name, oForeignKey);
      end;
      /// <summary>
      /// Coluna tabela master
      /// </summary>
      oFromField := TColumnMIK.Create(ATable);
      oFromField.Name := VarToStr(oDBResultSet.GetFieldValue('from'));
      oForeignKey.FromFields.Add(oFromField.Name, oFromField);
      /// <summary>
      /// Coluna tabela filha
      /// </summary>
      oToField := TColumnMIK.Create(ATable);
      oToField.Name := VarToStr(oDBResultSet.GetFieldValue('to'));
      oForeignKey.ToFields.Add(oToField.Name, oToField);
    end;
  except
  end;
end;

procedure TCatalogMetadataSQLite.GetFunctions;
begin
  inherited;

end;

procedure TCatalogMetadataSQLite.GetProcedures;
begin
  inherited;

end;

procedure TCatalogMetadataSQLite.GetSequences;
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

procedure TCatalogMetadataSQLite.GetTriggers(ATable: TTableMIK);
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

procedure TCatalogMetadataSQLite.GetIndexeKeys(ATable: TTableMIK);
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
      oColumn.Name := VarToStr(oDBResultSet.GetFieldValue('name'));
      AIndexeKey.Fields.Add(oColumn.Name, oColumn);
    end;
  end;

begin
  inherited;
  FSQLText := GetSelectIndexe(ATable.Name);
  oDBResultSet := Execute;
  while oDBResultSet.NotEof do
  begin
    if VarAsType(oDBResultSet.GetFieldValue('origin'), varString) = 'pk' then
       Continue;
    oIndexeKey := TIndexeKeyMIK.Create(ATable);
    oIndexeKey.Name := VarToStr(oDBResultSet.GetFieldValue('name'));
    oIndexeKey.Unique := VarAsType(oDBResultSet.GetFieldValue('unique'), varInteger) = 1;
    ATable.IndexeKeys.Add(UpperCase(oIndexeKey.Name), oIndexeKey);
    /// <summary>
    /// Gera a lista de campos do indexe
    /// </summary>
    GetIndexeKeyColumns(oIndexeKey);
  end;
end;

procedure TCatalogMetadataSQLite.GetViews;
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
    oView.Name := VarToStr(oDBResultSet.GetFieldValue('name'));
    oView.Description := '';
    oView.Script := VarToStr(oDBResultSet.GetFieldValue('sql'));
    FCatalogMetadata.Views.Add(UpperCase(oView.Name), oView);
  end;
end;

procedure TCatalogMetadataSQLite.ResolveFieldType(AColumn: TColumnMIK; ATypeName: string);
var
  iPos1, iPos2: Integer;
  sDefArgs: string;

  procedure SetPrecScale(ADefPrec, ADefScale: Integer);
  var
    sSize, sPrecision, sCale: string;
    iPos: Integer;
  begin
    iPos := Pos(',', sDefArgs);
    if iPos = 0 then
      AColumn.Size := StrToIntDef(sDefArgs, ADefPrec)
    else
    begin
      sPrecision := Copy(sDefArgs, 1, iPos - 1);
      sCale := Copy(sDefArgs, iPos + 1, Length(sDefArgs));
      AColumn.Precision := StrToIntDef(sPrecision, ADefScale);
      AColumn.Scale := StrToIntDef(sCale, ADefPrec);
    end;
  end;

begin
  AColumn.FieldType := ftUnknown;
  AColumn.Size := 0;
  AColumn.Precision := 0;
  ATypeName := Trim(UpperCase(ATypeName));

  sDefArgs := '';

  iPos1 := Pos('(', ATypeName);
  iPos2 := Pos(')', ATypeName);
  if iPos1 > 0  then
  begin
    sDefArgs := Copy(ATypeName, iPos1 + 1, iPos2 - iPos1 - 1);
    ATypeName := Copy(ATypeName, 1, iPos1 - 1);
    SetPrecScale(0, 0);
  end;
  AColumn.TypeName := ATypeName;
  SetFieldType(AColumn);
  /// <summary>
  /// Resolve Field Type
  /// </summary>
  GetFieldTypeDefinition(AColumn);
end;

function TCatalogMetadataSQLite.GetSelectForeignKey(ATableName: string): string;
begin
  Result := Format('PRAGMA foreign_key_list("%s")', [ATableName]);
end;

function TCatalogMetadataSQLite.GetSelectIndexe(ATableName: string): string;
begin
 Result := Format('PRAGMA index_list("%s")', [ATableName]);
end;

function TCatalogMetadataSQLite.GetSelectIndexeColumns(AIndexeName: string): string;
begin
  Result := Format('PRAGMA index_info("%s")', [AIndexeName]);
end;

function TCatalogMetadataSQLite.GetSelectPrimaryKey(ATableName: string): string;
begin
  Result := Format('PRAGMA table_info("%s")', [ATableName]);
end;

function TCatalogMetadataSQLite.GetSelectSequences: string;
begin
  Result := ' select name ' +
            ' from sqlite_sequence ' +
            ' order by name';
end;

function TCatalogMetadataSQLite.GetSelectTableColumns(ATableName: string): string;
begin
  Result := Format('PRAGMA table_info("%s")', [ATableName]);
end;

function TCatalogMetadataSQLite.GetSelectTables: string;
begin
  Result := ' select name ' +
            ' from sqlite_master ' +
            ' where type = ''table'' ' +
            ' and tbl_name not like ''sqlite_%'' ' +
            ' order by name ';
end;

function TCatalogMetadataSQLite.GetSelectTriggers(ATableName: string): string;
begin
  Result := ' select name ' +
            ' from sqlite_master ' +
            ' where type = ''trigger'' ' +
            ' and tbl_name = ''' + ATableName + ''' ' +
            ' order by name ';
end;

function TCatalogMetadataSQLite.GetSelectViews: string;
begin
  Result := ' select name ' +
            ' from sqlite_master ' +
            ' where type = ''view'' ' +
            ' and tbl_name not like ''sqlite_%'' ' +
            ' order by name ';
end;

initialization
  TMetadataRegister.GetInstance.RegisterMetadata(dnSQLite, TCatalogMetadataSQLite.Create);

end.

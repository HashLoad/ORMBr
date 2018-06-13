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

unit ormbr.metadata.absolutedb;

interface

uses
  DB,
  Classes,
  SysUtils,
  StrUtils,
  Variants,
  Generics.Collections,
  ABSMain,
  ABSTypes,
  ormbr.metadata.register,
  ormbr.metadata.extract,
  ormbr.database.mapping,
  ormbr.factory.interfaces,
  ormbr.mapping.rttiutils,
  ormbr.types.database;

type
  TCatalogMetadataAbsoluteDB = class(TCatalogMetadataAbstract)
  strict private
    procedure ResolveFieldType(AColumn: TColumnMIK; ADataType: TABSAdvancedFieldType);
  protected
    function Execute: IDBResultSet;
  public
    procedure GetCatalogs; override;
    procedure GetSchemas; override;
    procedure GetTables; override;
    procedure GetColumns(ATable: TTableMIK); override;
    procedure GetPrimaryKey(ATable: TTableMIK); override;
    procedure GetIndexeKeys(ATable: TTableMIK); override;
    procedure GetSequences; override;
    procedure GetDatabaseMetadata; override;
  end;

implementation

{ TSchemaExtractSQLite }

function TCatalogMetadataAbsoluteDB.Execute: IDBResultSet;
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

procedure TCatalogMetadataAbsoluteDB.GetDatabaseMetadata;
begin
  inherited;
  GetCatalogs;
end;

procedure TCatalogMetadataAbsoluteDB.GetCatalogs;
begin
  inherited;
  FCatalogMetadata.Name := '';
  GetSchemas;
end;

procedure TCatalogMetadataAbsoluteDB.GetSchemas;
begin
  inherited;
  FCatalogMetadata.Schema := '';
  GetTables;
end;

procedure TCatalogMetadataAbsoluteDB.GetTables;
var
  oTableList: TStringList;
  oTable: TTableMIK;
  iFor: Integer;
begin
  inherited;
  oTableList := TStringList.Create;
  try
    TABSDatabase(FConnection).GetTablesList(oTableList);
    for iFor := 0 to oTableList.Count - 1 do
    begin
      oTable := TTableMIK.Create(FCatalogMetadata);
      oTable.Name := oTableList[iFor];
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
      /// Extrair Indexes da tabela
      /// </summary>
      GetIndexeKeys(oTable);
      /// <summary>
      /// Adiciona na lista de tabelas extraidas
      /// </summary>
      FCatalogMetadata.Tables.Add(UpperCase(oTable.Name), oTable);
    end;
  finally
    oTableList.Free;
  end;
end;

procedure TCatalogMetadataAbsoluteDB.GetColumns(ATable: TTableMIK);
var
  oABSTable: TABSTable;
  oColumn: TColumnMIK;
  iFor: Integer;
begin
  inherited;
  oABSTable.Close;
  oABSTable.DatabaseName := (FConnection as TABSDatabase).DatabaseName;
  oABSTable.TableName := ATable.Name;
  oABSTable.FieldDefs.Update;

  for IFor := 0 to oABSTable.AdvFieldDefs.Count - 1 do
  begin
    oColumn := TColumnMIK.Create(ATable);
    oColumn.Name := oABSTable.AdvFieldDefs[IFor].Name;
    oColumn.Description := oColumn.Name;
    oColumn.Position := iFor + 1;
    /// <summary>
    /// O método ResolveTypeField() extrai e popula as propriedades relacionadas abaixo
    /// </summary>
    /// <param name="AColumn: TColumnMIK">Informar [oColumn] para receber informações adicionais
    /// </param>
    /// <param name="ATypeName: String">Campo com descriçao do tipo que veio na extração do metadata
    /// </param>
    /// <remarks>
    /// Relação das propriedades que serão alimentadas no método ResolveTypeField()
    /// oColumn.FieldType: TTypeField;
    /// oColumn.TypeName: string;
    /// oColumn.Size: Integer;
    /// oColumn.Precision: Integer;
    /// oColumn.Scale: Integer;
    /// </remarks>
    ResolveFieldType(oColumn, oABSTable.AdvFieldDefs[IFor].DataType);
    ///
    oColumn.Size := oABSTable.AdvFieldDefs[IFor].Size;
    oColumn.NotNull := oABSTable.AdvFieldDefs[IFor].Required;
    oColumn.DefaultValue := oABSTable.AdvFieldDefs[IFor].DefaultValue.AsVariant;
    oColumn.AutoIncrement := (Pos('AutoInc', oColumn.TypeName) > 0);

    ATable.Fields.Add(FormatFloat('000000', oColumn.Position), oColumn);
  end;
end;

procedure TCatalogMetadataAbsoluteDB.GetPrimaryKey(ATable: TTableMIK);
{
var
  oDBResultSet: IDBResultSet;

  function GetColumnAutoIncrement(ATableName: string): integer;
  var
    oDBResultSet: IDBResultSet;
  begin
    FSQLText := ' select count(*) as autoinc ' +
                ' from sqlite_sequence ' +
                ' where name = ''' + ATableName + ''' ' +
                ' order by name';
    oDBResultSet := Execute;
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
//      oColumn.AutoIncrement := GetColumnAutoIncrement(ATable.Name) > 0;
      APrimaryKey.Fields.Add(FormatFloat('000000', oColumn.Position), oColumn);
    end;
  end;
}
begin
  inherited;
{
  FSQLText := GetSelectPrimaryKey(ATable.Name);
  oDBResultSet := Execute;
  while oDBResultSet.NotEof do
  begin
    if VarAsType(oDBResultSet.GetFieldValue('pk'), varInteger) = 1 then
    begin
      ATable.PrimaryKey.Name := Format('PK_%s', [ATable.Name]);
      ATable.PrimaryKey.Description := '';
      ATable.PrimaryKey.AutoIncrement := GetColumnAutoIncrement(ATable.Name) > 0;
      /// <summary>
      /// Estrai as columnas da primary key
      /// </summary>
      GetPrimaryKeyColumns(ATable.PrimaryKey);
      Break
    end;
  end;
}
end;

procedure TCatalogMetadataAbsoluteDB.GetSequences;
{
var
  oDBResultSet: IDBResultSet;
  oSequence: TSequenceMIK;
}
begin
  inherited;
{
  FSQLText := GetSelectSequences;
  oDBResultSet := Execute;
  while oDBResultSet.NotEof do
  begin
    oSequence := TSequenceMIK.Create(FCatalogMetadata);
    oSequence.Name := VarToStr(oDBResultSet.GetFieldValue('name'));
    oSequence.Description := VarToStr(oDBResultSet.GetFieldValue('description'));;
    FCatalogMetadata.Sequences.Add(UpperCase(oSequence.Name), oSequence);
  end;
}
end;

procedure TCatalogMetadataAbsoluteDB.GetIndexeKeys(ATable: TTableMIK);
{
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
}
begin
  inherited;
{
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
}
end;

procedure TCatalogMetadataAbsoluteDB.ResolveFieldType(AColumn: TColumnMIK;
  ADataType: TABSAdvancedFieldType);
var
  sTypeName: string;
begin
  AColumn.FieldType := ftUnknown;
  AColumn.Precision := 0;
  case ADataType of
    aftChar:            begin AColumn.FieldType := ftFixedChar; AColumn.TypeName := 'CHAR' end;
    aftString:          begin AColumn.FieldType := ftString; AColumn.TypeName := 'VARCHAR' end;
    aftWideChar:        begin AColumn.FieldType := ftWideString; AColumn.TypeName := 'WIDESTRING' end;
    aftWideString:      begin AColumn.FieldType := ftWideString; AColumn.TypeName := 'WIDESTRING' end;
    aftShortint:        begin AColumn.FieldType := ftSmallint; AColumn.TypeName := 'SMALLINT' end;
    aftSmallint:        begin AColumn.FieldType := ftSmallint; AColumn.TypeName := 'SMALLINT' end;
    aftInteger:         begin AColumn.FieldType := ftInteger; AColumn.TypeName := 'INTEGER' end;
    aftLargeint:        begin AColumn.FieldType := ftLargeint; AColumn.TypeName := 'LARGEINT' end;
    aftByte:            begin AColumn.FieldType := ftByte; AColumn.TypeName := 'BYTE' end;
    aftWord:            begin AColumn.FieldType := ftWord; AColumn.TypeName := 'WORD' end;
    aftCardinal:        begin AColumn.FieldType := ftWord; AColumn.TypeName := 'CARDINAL' end;
    aftAutoInc:         begin AColumn.FieldType := ftInteger; AColumn.TypeName := 'AUTOINC' end;
    aftAutoIncShortint: begin AColumn.FieldType := ftShortint; AColumn.TypeName := 'AUTOINC(SHORTINT)' end;
    aftAutoIncSmallint: begin AColumn.FieldType := ftSmallint; AColumn.TypeName := 'AUTOINC(SMALLINT)' end;
    aftAutoIncInteger:  begin AColumn.FieldType := ftInteger; AColumn.TypeName := 'AUTOINC(INTEGER)' end;
    aftAutoIncLargeint: begin AColumn.FieldType := ftLargeint; AColumn.TypeName := 'AUTOINC(LARGEINT)' end;
    aftAutoIncByte:     begin AColumn.FieldType := ftByte; AColumn.TypeName := 'AUTOINC(BYTE)' end;
    aftAutoIncWord:     begin AColumn.FieldType := ftWord; AColumn.TypeName := 'AUTOINC(WORD)' end;
    aftAutoIncCardinal: begin AColumn.FieldType := ftInteger; AColumn.TypeName := 'AUTOINC(CARDINAL)' end;
    aftSingle:          begin AColumn.FieldType := ftSingle; AColumn.TypeName := 'SINGLE' end;
    aftDouble:          begin AColumn.FieldType := ftFloat; AColumn.TypeName := 'FLOAT' end;
    aftExtended:        begin AColumn.FieldType := ftBCD; AColumn.TypeName := 'EXTENDED' end;
    aftBoolean:         begin AColumn.FieldType := ftBoolean; AColumn.TypeName := 'BOOLEAN' end;
    aftCurrency:        begin AColumn.FieldType := ftCurrency; AColumn.TypeName := 'CURRENCY' end;
    aftDate:            begin AColumn.FieldType := ftDate; AColumn.TypeName := 'DATE' end;
    aftTime:            begin AColumn.FieldType := ftTime; AColumn.TypeName := 'TIME' end;
    aftDateTime:        begin AColumn.FieldType := ftDateTime; AColumn.TypeName := 'DATETIME' end;
    aftTimeStamp:       begin AColumn.FieldType := ftTimeStamp; AColumn.TypeName := 'TIMESTAMP' end;
    aftBytes:           begin AColumn.FieldType := ftBytes; AColumn.TypeName := 'BYTES' end;
    aftVarBytes:        begin AColumn.FieldType := ftVarBytes; AColumn.TypeName := 'VARBYTES' end;
    aftBlob:            begin AColumn.FieldType := ftBlob; AColumn.TypeName := 'BLOB' end;
    aftGraphic:         begin AColumn.FieldType := ftGraphic; AColumn.TypeName := 'GRAPHIC' end;
    aftMemo:            begin AColumn.FieldType := ftMemo; AColumn.TypeName := 'MEMO' end;
    aftFormattedMemo:   begin AColumn.FieldType := ftWideMemo; AColumn.TypeName := 'FORMATTEDMEMO' end;
    aftWideMemo:        begin AColumn.FieldType := ftWideMemo; AColumn.TypeName := 'WIDEMEMO' end;
    aftGuid:            begin AColumn.FieldType := ftGuid; AColumn.TypeName := 'GUID' end;
  end;
end;

initialization
  TMetadataRegister.GetInstance.RegisterMetadata(dnAbsoluteDB, TCatalogMetadataAbsoluteDB.Create);

end.


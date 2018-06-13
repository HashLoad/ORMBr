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

unit ormbr.database.factory;

interface

uses
  DB,
  SysUtils,
  Generics.Collections,
  ormbr.ddl.interfaces,
  ormbr.database.abstract,
  ormbr.factory.interfaces,
  ormbr.database.mapping,
  ormbr.ddl.commands,
  ormbr.types.database;

type
  TDatabaseFactory = class(TDatabaseAbstract)
  private
    procedure CompareTables(AMasterDB, ATargetDB: TCatalogMetadataMIK);
    procedure CompareViews(AMasterDB, ATargetDB: TCatalogMetadataMIK);
    procedure CompareSequences(AMasterDB, ATargetDB: TCatalogMetadataMIK);
    procedure CompareTablesForeignKeys(AMasterDB, ATargetDB: TCatalogMetadataMIK);
    procedure CompareForeignKeys(AMasterTable, ATargetTable: TTableMIK);
    procedure CompareColumns(AMasterTable, ATargetTable: TTableMIK);
    procedure ComparePrimaryKey(AMasterTable, ATargetTable: TTableMIK);
    procedure CompareIndexes(AMasterTable, ATargetTable: TTableMIK);
    procedure CompareTriggers(AMasterTable, ATargetTable: TTableMIK);
    procedure CompareChecks(AMasterTable, ATargetTable: TTableMIK);
    procedure ActionCreateTable(ATable: TTableMIK);
    procedure ActionCreateIndexe(AIndexe: TIndexeKeyMIK);
    procedure ActionCreateCheck(ACheck: TCheckMIK);
    procedure ActionCreatePrimaryKey(APrimaryKey: TPrimaryKeyMIK);
    procedure ActionCreateColumn(AColumn: TColumnMIK);
    procedure ActionCreateSequence(ASequence: TSequenceMIK);
    procedure ActionCreateForeignKey(AForeignKey: TForeignKeyMIK);
    procedure ActionCreateView(AView: TViewMIK);
    procedure ActionDropTable(ATable: TTableMIK);
    procedure ActionDropColumn(AColumn: TColumnMIK);
    procedure ActionDropPrimaryKey(APrimaryKey: TPrimaryKeyMIK);
    procedure ActionDropSequence(ASequence: TSequenceMIK);
    procedure ActionDropIndexe(AIndexe: TIndexeKeyMIK);
    procedure ActionDropForeignKey(AForeignKey: TForeignKeyMIK);
    procedure ActionDropCheck(ACheck: TCheckMIK);
    procedure ActionDropView(AView: TViewMIK);
    procedure ActionAlterColumn(AColumn: TColumnMIK);
    procedure ActionDropDefaultValue(AColumn: TColumnMIK);
    procedure ActionAlterDefaultValue(AColumn: TColumnMIK);
    procedure ActionAlterCheck(ACheck: TCheckMIK);
    procedure ActionEnableForeignKeys(AEnable: Boolean);
    procedure ActionEnableTriggers(AEnable: Boolean);
    function DeepEqualsColumn(AMasterColumn, ATargetColumn: TColumnMIK): Boolean;
    function DeepEqualsDefaultValue(AMasterColumn, ATargetColumn: TColumnMIK): Boolean;
    function DeepEqualsForeignKey(AMasterForeignKey, ATargetForeignKey: TForeignKeyMIK): Boolean;
    function DeepEqualsForeignKeyFromColumns(AMasterForeignKey, ATargetForeignKey: TForeignKeyMIK): Boolean;
    function DeepEqualsForeignKeyToColumns(AMasterForeignKey, ATargetForeignKey: TForeignKeyMIK): Boolean;
    function DeepEqualsIndexe(AMasterIndexe, ATargetIndexe: TIndexeKeyMIK): Boolean;
    function DeepEqualsIndexeColumns(AMasterIndexe, ATargetIndexe: TIndexeKeyMIK): Boolean;
  protected
    function GetFieldTypeValid(AFieldType: TFieldType): TFieldType; override;
    procedure GenerateDDLCommands(AMasterDB, ATargetDB: TCatalogMetadataMIK); override;
  public
    procedure BuildDatabase; override;
  end;

implementation

{ TDatabaseFactory }

procedure TDatabaseFactory.BuildDatabase;
begin
  inherited;
  FCatalogMaster := TCatalogMetadataMIK.Create;
  FCatalogTarget := TCatalogMetadataMIK.Create;
  try
    /// <summary>
    /// Extrai o metadata com base nos modelos existentes e no banco de dados
    /// </summary>
    ExtractDatabase;
    /// <summary>
    /// Gera os comandos DDL para atualização do banco da dados.
    /// </summary>
    GenerateDDLCommands(FCatalogMaster, FCatalogTarget);
  finally
    FCatalogMaster.Free;
    FCatalogTarget.Free;
  end;
end;

function TDatabaseFactory.DeepEqualsColumn(AMasterColumn, ATargetColumn: TColumnMIK): Boolean;
begin
  Result := True;
  if AMasterColumn.TypeName <> ATargetColumn.TypeName then
    Exit(False);
  if AMasterColumn.Size <> ATargetColumn.Size then
    Exit(False);
  if AMasterColumn.Precision <> ATargetColumn.Precision then
    Exit(False);
  if AMasterColumn.NotNull <> ATargetColumn.NotNull then
    Exit(False);
  if AMasterColumn.AutoIncrement <> ATargetColumn.AutoIncrement then
    Exit(False);
  if AMasterColumn.SortingOrder <> ATargetColumn.SortingOrder then
    Exit(False);
  if AMasterColumn.DefaultValue <> ATargetColumn.DefaultValue then
    Exit(False);
  if GetFieldTypeValid(AMasterColumn.FieldType) <> GetFieldTypeValid(ATargetColumn.FieldType) then
    Exit(False);
  if ComparerFieldPosition then
    if (AMasterColumn.Position <> ATargetColumn.Position) then
      Exit(False);
//  if AMasterColumn.Description <> ATargetColumn.Description then
//    Exit(False);
end;

function TDatabaseFactory.DeepEqualsDefaultValue(AMasterColumn, ATargetColumn: TColumnMIK): Boolean;
begin
  Result := True;
  if AMasterColumn.DefaultValue <> ATargetColumn.DefaultValue then
    Exit(False);
end;

function TDatabaseFactory.DeepEqualsForeignKey(AMasterForeignKey, ATargetForeignKey: TForeignKeyMIK): Boolean;
begin
  Result := True;
  if not SameText(AMasterForeignKey.FromTable, ATargetForeignKey.FromTable) then
    Exit(False);
  if AMasterForeignKey.OnDelete <> ATargetForeignKey.OnDelete then
    Exit(False);
  if AMasterForeignKey.OnUpdate <> ATargetForeignKey.OnUpdate then
    Exit(False);
//  if AMasterForeignKey.Description <> ATargetForeignKey.Description then
//    Exit(False);
end;

function TDatabaseFactory.DeepEqualsIndexe(AMasterIndexe, ATargetIndexe: TIndexeKeyMIK): Boolean;
begin
  Result := True;
  if AMasterIndexe.Unique <> ATargetIndexe.Unique then
    Exit(False);
//  if AMasterIndexe.Description <> ATargetIndexe.Description then
//    Exit(False);
end;

procedure TDatabaseFactory.GenerateDDLCommands(AMasterDB, ATargetDB: TCatalogMetadataMIK);
begin
  inherited;
  FDDLCommands.Clear;
  /// <summary>
  /// Gera script que desabilita todas as ForeignKeys
  /// </summary>
  ActionEnableForeignKeys(False);
  /// <summary>
  /// Gera script que desabilita todas as Triggers
  /// </summary>
  ActionEnableTriggers(False);
  /// <summary>
  /// Compara Tabelas
  /// </summary>
  CompareTables(AMasterDB, ATargetDB);
  /// <summary>
  /// Compara Views
  /// </summary>
  CompareViews(AMasterDB, ATargetDB);
  /// <summary>
  /// Compara Sequences
  /// </summary>
  CompareSequences(AMasterDB, ATargetDB);
  /// <summary>
  /// Compara ForeingKeys
  /// </summary>
  CompareTablesForeignKeys(AMasterDB, ATargetDB);
  /// <summary>
  /// Gera script que habilita todas as ForeignKeys
  /// </summary>
  ActionEnableForeignKeys(True);
  /// <summary>
  /// Gera script que habilita todas as Triggers
  /// </summary>
  ActionEnableTriggers(True);
  /// <summary>
  /// Execute Commands
  /// </summary>
  ExecuteDDLCommands;
end;

function TDatabaseFactory.GetFieldTypeValid(AFieldType: TFieldType): TFieldType;
begin
  if AFieldType in [ftCurrency, ftFloat, ftBCD, ftExtended, ftSingle, ftFMTBcd] then
    Result := ftCurrency
  else
  if AFieldType in [ftString, ftFixedChar, ftWideString, ftFixedWideChar] then
    Result := ftString
  else
  if AFieldType in [ftInteger, ftShortint, ftSmallint, ftLargeint] then
    Result := ftInteger
  else
  if AFieldType in [ftMemo, ftFmtMemo, ftWideMemo] then
    Result := ftMemo
  else
    Result := AFieldType;
end;

procedure TDatabaseFactory.CompareTables(AMasterDB, ATargetDB: TCatalogMetadataMIK);
var
  oTableMaster: TPair<string, TTableMIK>;
  oTableTarget: TPair<string, TTableMIK>;
begin
  /// <summary>
  /// Gera script de exclusão de tabela, caso não exista um modelo para ela no banco.
  /// </summary>
  for oTableTarget in ATargetDB.Tables do
  begin
    if not AMasterDB.Tables.ContainsKey(oTableTarget.Key) then
      ActionDropTable(oTableTarget.Value);
  end;
  /// <summary>
  /// Gera script de criação de tabela, caso a tabela do modelo não exista no banco.
  /// </summary>
  for oTableMaster in AMasterDB.Tables do
  begin
    if ATargetDB.Tables.ContainsKey(oTableMaster.Key) then
    begin
      /// <summary>
      /// Table Columns
      /// </summary>
      CompareColumns(oTableMaster.Value, ATargetDB.Tables.Items[oTableMaster.Key]);

      /// <summary>
      /// Table PrimaryKey
      /// </summary>
      if (oTableMaster.Value.PrimaryKey.Fields.Count > 0) or
         (ATargetDB.Tables.Items[oTableMaster.Key].PrimaryKey.Fields.Count > 0) then
        ComparePrimaryKey(oTableMaster.Value, ATargetDB.Tables.Items[oTableMaster.Key]);

      /// <summary>
      /// Table Indexes
      /// </summary>
      if (oTableMaster.Value.IndexeKeys.Count > 0) or
         (ATargetDB.Tables.Items[oTableMaster.Key].IndexeKeys.Count > 0) then
        CompareIndexes(oTableMaster.Value, ATargetDB.Tables.Items[oTableMaster.Key]);

      /// <summary>
      /// Table Checks
      /// </summary>
      if (oTableMaster.Value.Checks.Count > 0) or
         (ATargetDB.Tables.Items[oTableMaster.Key].Checks.Count > 0) then
        CompareChecks(oTableMaster.Value, ATargetDB.Tables.Items[oTableMaster.Key]);

      /// <summary>
      /// Table Triggers
      /// </summary>
      if (oTableMaster.Value.Triggers.Count > 0) or
         (ATargetDB.Tables.Items[oTableMaster.Key].Triggers.Count > 0) then
        CompareTriggers(oTableMaster.Value, ATargetDB.Tables.Items[oTableMaster.Key]);
    end
    else
      ActionCreateTable(oTableMaster.Value);
  end;
end;

procedure TDatabaseFactory.CompareTriggers(AMasterTable, ATargetTable: TTableMIK);
var
  oTriggerMaster: TPair<string, TTriggerMIK>;
  oTriggerTarget: TPair<string, TTriggerMIK>;
begin
  if TSupportedFeature.Triggers in FGeneratorCommand.SupportedFeatures then
  begin
    /// <summary>
    /// Remove trigger que não existe no modelo.
    /// </summary>
    for oTriggerTarget in ATargetTable.Triggers do
    begin
      if not AMasterTable.Triggers.ContainsKey(oTriggerTarget.Key) then
  //      ActionDropTrigger(oTriggerTarget.Value);
    end;
    /// <summary>
    /// Gera script de criação de trigger, caso a trigger do modelo não exista no banco.
    /// </summary>
    for oTriggerMaster in AMasterTable.Triggers do
    begin
      if ATargetTable.Triggers.ContainsKey(oTriggerMaster.Key) then
//        CompareTriggerScript(oTriggerMaster.Value, ATargetTable.Triggers.Items[oTriggerMaster.Key])
      else
//        ActionCreateTrigger(oTriggerMaster.Value);
    end;
  end;
end;

procedure TDatabaseFactory.CompareViews(AMasterDB, ATargetDB: TCatalogMetadataMIK);
var
  oViewMaster: TPair<string, TViewMIK>;
  oViewTarget: TPair<string, TViewMIK>;
begin
  if TSupportedFeature.Triggers in FGeneratorCommand.SupportedFeatures then
  begin
    /// <summary>
    /// Gera script de exclusão da view, caso não exista um modelo para ela no banco.
    /// </summary>
    for oViewTarget in ATargetDB.Views do
    begin
      if not AMasterDB.Views.ContainsKey(oViewTarget.Key) then
        ActionDropView(oViewTarget.Value);
    end;
    /// <summary>
    /// Gera script de criação da view, caso a view do modelo não exista no banco.
    /// </summary>
    for oViewMaster in AMasterDB.Views do
    begin
      if ATargetDB.Views.ContainsKey(oViewMaster.Key) then
      begin
        oViewTarget.Value := oViewMaster.Value;

        if CompareText(oViewMaster.Value.Script, oViewTarget.Value.Script) <> 0 then
          ActionDropView(oViewTarget.Value);
      end
      else
        ActionCreateView(oViewMaster.Value);
    end;
  end;
end;

procedure TDatabaseFactory.CompareChecks(AMasterTable, ATargetTable: TTableMIK);
var
  oCheckMaster: TPair<string, TCheckMIK>;
  oCheckTarget: TPair<string, TCheckMIK>;
begin
  if TSupportedFeature.Checks in FGeneratorCommand.SupportedFeatures then
  begin
     for oCheckTarget in ATargetTable.Checks do
     begin
       if not AMasterTable.Checks.ContainsKey(oCheckTarget.Key) then
         ActionDropCheck(oCheckTarget.Value);
     end;

     for oCheckMaster in AMasterTable.Checks do
     begin
       if ATargetTable.Checks.ContainsKey(oCheckMaster.Key) then
         ActionDropCheck(oCheckTarget.Value);

       ActionAlterCheck(oCheckMaster.Value);
     end;
  end;
end;

procedure TDatabaseFactory.CompareColumns(AMasterTable, ATargetTable: TTableMIK);
var
  oColumnMaster: TPair<string, TColumnMIK>;
  oColumnTarget: TPair<string, TColumnMIK>;
  oColumn: TColumnMIK;

  function ExistMasterColumn(AColumnName: string): TColumnMIK;
  var
    oColumn: TColumnMIK;
  begin
    Result := nil;
    for oColumn in AMasterTable.Fields.Values do
      if SameText(oColumn.Name, AColumnName) then
        Exit(oColumn);
  end;

  function ExistTargetColumn(AColumnName: string): TColumnMIK;
  var
    oColumn: TColumnMIK;
  begin
    Result := nil;
    for oColumn in ATargetTable.Fields.Values do
      if SameText(oColumn.Name, AColumnName) then
        Exit(oColumn);
  end;

begin
  /// <summary>
  /// Remove coluna que não existe no modelo.
  /// </summary>
  for oColumnTarget in ATargetTable.FieldsSort do
  begin
    oColumn := ExistMasterColumn(oColumnTarget.Value.Name);
    if oColumn = nil then
      ActionDropColumn(oColumnTarget.Value);
  end;
  /// <summary>
  /// Adiciona coluna do modelo que não exista no banco
  /// Compara coluna que exista no modelo e no banco
  /// </summary>
  for oColumnMaster in AMasterTable.FieldsSort do
  begin
    oColumn := ExistTargetColumn(oColumnMaster.Value.Name);
    if oColumn = nil then
      ActionCreateColumn(oColumnMaster.Value)
    else
    begin
      if not DeepEqualsColumn(oColumnMaster.Value, oColumn) then
        ActionAlterColumn(oColumnMaster.Value);

      /// <summary>
      /// Compara DefaultValue
      /// </summary>
      if not DeepEqualsDefaultValue(oColumnMaster.Value, oColumn) then
      begin
        if Length(oColumnMaster.Value.DefaultValue) > 0 then
          ActionAlterDefaultValue(oColumnMaster.Value)
        else
          ActionDropDefaultValue(oColumn);
      end;
    end;
  end;
end;

procedure TDatabaseFactory.CompareTablesForeignKeys(AMasterDB, ATargetDB: TCatalogMetadataMIK);
var
  oTableMaster: TPair<string, TTableMIK>;
  oForeignKeyMaster: TPair<string, TForeignKeyMIK>;
begin
  /// <summary>
  /// Gera script de criação das ForeingnKeys, caso não exista no banco.
  /// </summary>
  for oTableMaster in AMasterDB.Tables do
  begin
    if ATargetDB.Tables.ContainsKey(oTableMaster.Key) then
    begin
      /// <summary>
      /// Table ForeignKeys
      /// </summary>
      if (oTableMaster.Value.ForeignKeys.Count > 0) or
         (ATargetDB.Tables.Items[oTableMaster.Key].ForeignKeys.Count > 0) then
        CompareForeignKeys(oTableMaster.Value, ATargetDB.Tables.Items[oTableMaster.Key]);
    end
    else
    begin
      /// <summary>
      /// Gera script de criação dos ForeignKey da nova tabela.
      /// </summary>
      if FDriverName <> dnSQLite then
        for oForeignKeyMaster in oTableMaster.Value.ForeignKeys do
          ActionCreateForeignKey(oForeignKeyMaster.Value);
    end;
  end;
end;

function TDatabaseFactory.DeepEqualsForeignKeyFromColumns(AMasterForeignKey, ATargetForeignKey: TForeignKeyMIK): Boolean;
var
  oColumnMaster: TPair<string, TColumnMIK>;
  oColumnTarget: TPair<string, TColumnMIK>;
  oColumn: TColumnMIK;

  function ExistMasterFromColumn(AColumnName: string): TColumnMIK;
  var
    oColumn: TPair<string, TColumnMIK>;
  begin
    Result := nil;
    for oColumn in AMasterForeignKey.FromFields do
      if SameText(oColumn.Value.Name, AColumnName) then
        Exit(oColumn.Value);
  end;

  function ExistTargetFromColumn(AColumnName: string): TColumnMIK;
  var
    oColumn: TPair<string, TColumnMIK>;
  begin
    Result := nil;
    for oColumn in ATargetForeignKey.FromFields do
      if SameText(oColumn.Value.Name, AColumnName) then
        Exit(oColumn.Value);
  end;

begin
  Result := True;
  /// <summary>
  /// Comparação dos campos dos indexes banco/modelo
  /// </summary>
  for oColumnTarget in ATargetForeignKey.FromFieldsSort do
  begin
    oColumn := ExistMasterFromColumn(oColumnTarget.Value.Name);
    if oColumn = nil then
      Exit(False)
  end;
  /// <summary>
  /// Comparação dos campos dos indexes modelo/banco
  /// </summary>
  for oColumnMaster in AMasterForeignKey.FromFieldsSort do
  begin
    oColumn := ExistTargetFromColumn(oColumnMaster.Value.Name);
    if oColumn = nil then
      Exit(False)
    else
    begin
      if not DeepEqualsColumn(oColumnMaster.Value, oColumn) then
        Exit(False);
    end;
  end;
end;

procedure TDatabaseFactory.CompareForeignKeys(AMasterTable, ATargetTable: TTableMIK);
var
  oForeignKeyMaster: TPair<string, TForeignKeyMIK>;
  oForeignKeyTarget: TPair<string, TForeignKeyMIK>;
begin
  if TSupportedFeature.ForeignKeys in FGeneratorCommand.SupportedFeatures then
  begin
    /// <summary>
    /// Remove indexe que não existe no modelo.
    /// </summary>
    for oForeignKeyTarget in ATargetTable.ForeignKeys do
    begin
      if not AMasterTable.ForeignKeys.ContainsKey(oForeignKeyTarget.Key) then
        ActionDropForeignKey(oForeignKeyTarget.Value);
    end;
    /// <summary>
    /// Gera script de criação de indexe, caso a indexe do modelo não exista no banco.
    /// </summary>
    for oForeignKeyMaster in AMasterTable.ForeignKeys do
    begin
      if ATargetTable.ForeignKeys.ContainsKey(oForeignKeyMaster.Key) then
      begin
        /// <summary>
        /// Checa diferença do ForeignKey
        /// </summary>
        oForeignKeyTarget.Value := ATargetTable.ForeignKeys.Items[oForeignKeyMaster.Key]; 
        if (not DeepEqualsForeignKey(oForeignKeyMaster.Value, oForeignKeyTarget.Value)) or
           (not DeepEqualsForeignKeyFromColumns(oForeignKeyMaster.Value, oForeignKeyTarget.Value)) or
           (not DeepEqualsForeignKeyToColumns  (oForeignKeyMaster.Value, oForeignKeyTarget.Value)) then
        begin
          ActionDropForeignKey(oForeignKeyTarget.Value);
          ActionCreateForeignKey(oForeignKeyMaster.Value);
        end;
      end
      else
        ActionCreateForeignKey(oForeignKeyMaster.Value);
    end;
  end;
end;

function TDatabaseFactory.DeepEqualsForeignKeyToColumns(AMasterForeignKey, ATargetForeignKey: TForeignKeyMIK): Boolean;
var
  oColumnMaster: TPair<string, TColumnMIK>;
  oColumnTarget: TPair<string, TColumnMIK>;
  oColumn: TColumnMIK;

  function ExistMasterToColumn(AColumnName: string): TColumnMIK;
  var
    oColumn: TPair<string, TColumnMIK>;
  begin
    Result := nil;
    for oColumn in AMasterForeignKey.ToFields do
      if SameText(oColumn.Value.Name, AColumnName) then
        Exit(oColumn.Value);
  end;

  function ExistTargetFromColumn(AColumnName: string): TColumnMIK;
  var
    oColumn: TPair<string, TColumnMIK>;
  begin
    Result := nil;
    for oColumn in ATargetForeignKey.ToFields do
      if SameText(oColumn.Value.Name, AColumnName) then
        Exit(oColumn.Value);
  end;

begin
  Result := True;
  /// <summary>
  /// Comparação dos campos dos indexes banco/modelo
  /// </summary>
  for oColumnTarget in ATargetForeignKey.ToFieldsSort do
  begin
    oColumn := ExistMasterToColumn(oColumnTarget.Value.Name);
    if oColumn = nil then
      Exit(False)
  end;
  /// <summary>
  /// Comparação dos campos dos indexes modelo/banco
  /// </summary>
  for oColumnMaster in AMasterForeignKey.ToFieldsSort do
  begin
    oColumn := ExistTargetFromColumn(oColumnMaster.Value.Name);
    if oColumn = nil then
      Exit(False)
    else
    begin
      if not DeepEqualsColumn(oColumnMaster.Value, oColumn) then
        Exit(False);
    end;
  end;
end;

function TDatabaseFactory.DeepEqualsIndexeColumns(AMasterIndexe, ATargetIndexe: TIndexeKeyMIK): Boolean;
var
  oColumnMaster: TPair<string, TColumnMIK>;
  oColumnTarget: TPair<string, TColumnMIK>;
  oColumn: TColumnMIK;

  function ExistMasterColumn(AColumnName: string): TColumnMIK;
  var
    oColumn: TPair<string, TColumnMIK>;
  begin
    Result := nil;
    for oColumn in AMasterIndexe.Fields do
      if SameText(oColumn.Value.Name, AColumnName) then
        Exit(oColumn.Value);
  end;

  function ExistTargetColumn(AColumnName: string): TColumnMIK;
  var
    oColumn: TPair<string, TColumnMIK>;
  begin
    Result := nil;
    for oColumn in ATargetIndexe.Fields do
      if SameText(oColumn.Value.Name, AColumnName) then
        Exit(oColumn.Value);
  end;

begin
  Result := True;
  /// <summary>
  /// Comparação dos campos dos indexes banco/modelo
  /// </summary>
  for oColumnTarget in ATargetIndexe.FieldsSort do
  begin
    oColumn := ExistMasterColumn(oColumnTarget.Value.Name);
    if oColumn = nil then
      Exit(False)
  end;
  /// <summary>
  /// Comparação dos campos dos indexes modelo/banco
  /// </summary>
  for oColumnMaster in AMasterIndexe.FieldsSort do
  begin
    oColumn := ExistTargetColumn(oColumnMaster.Value.Name);
    if oColumn = nil then
      Exit(False)
    else
    begin
      if not DeepEqualsColumn(oColumnMaster.Value, oColumn) then
        Exit(False);
    end;
  end;
end;

procedure TDatabaseFactory.CompareIndexes(AMasterTable, ATargetTable: TTableMIK);
var
  oIndexeMaster: TPair<string, TIndexeKeyMIK>;
  oIndexeTarget: TPair<string, TIndexeKeyMIK>;
begin
  /// <summary>
  /// Remove indexe que não existe no modelo.
  /// </summary>
  for oIndexeTarget in ATargetTable.IndexeKeys do
  begin
    if not AMasterTable.IndexeKeys.ContainsKey(oIndexeTarget.Key) then
      ActionDropIndexe(oIndexeTarget.Value);
  end;
  /// <summary>
  /// Gera script de criação de indexe, caso a indexe do modelo não exista no banco.
  /// </summary>
  for oIndexeMaster in AMasterTable.IndexeKeys do
  begin
    if ATargetTable.IndexeKeys.ContainsKey(oIndexeMaster.Key) then
    begin
      oIndexeTarget.Value := ATargetTable.IndexeKeys.Items[oIndexeMaster.Key];
      if (not DeepEqualsIndexe(oIndexeMaster.Value, oIndexeTarget.Value)) or
         (not DeepEqualsIndexeColumns(oIndexeMaster.Value, oIndexeTarget.Value)) then
      begin
        ActionDropIndexe(oIndexeTarget.Value);
        ActionCreateIndexe(oIndexeMaster.Value);
      end;
    end
    else
      ActionCreateIndexe(oIndexeMaster.Value);
  end;
end;

procedure TDatabaseFactory.ComparePrimaryKey(AMasterTable, ATargetTable: TTableMIK);
var
  oColumnMaster: TPair<string, TColumnMIK>;
  oColumn: TColumnMIK;

  function ExistTargetColumn(AColumnName: string): TColumnMIK;
  var
    oColumn: TPair<string, TColumnMIK>;
  begin
    Result := nil;
    for oColumn in ATargetTable.PrimaryKey.Fields do
      if SameText(oColumn.Value.Name, AColumnName) then
        Exit(oColumn.Value);
  end;

begin
  if not SameText(AMasterTable.PrimaryKey.Name, ATargetTable.PrimaryKey.Name) then
    ActionDropPrimaryKey(ATargetTable.PrimaryKey);

  /// <summary>
  /// Se alguma coluna não existir na PrimaryKey do banco recria a PrimaryKey.
  /// </summary>
  for oColumnMaster in AMasterTable.PrimaryKey.FieldsSort do
  begin
    oColumn := ExistTargetColumn(oColumnMaster.Value.Name);
    if oColumn = nil then
    begin
      ActionDropPrimaryKey(ATargetTable.PrimaryKey);
      ActionCreatePrimaryKey(AMasterTable.PrimaryKey);
    end;
  end;
end;

procedure TDatabaseFactory.CompareSequences(AMasterDB, ATargetDB: TCatalogMetadataMIK);
var
  oSequenceMaster: TPair<string, TSequenceMIK>;
  oSequenceTarget: TPair<string, TSequenceMIK>;
begin
  if TSupportedFeature.Sequences in FGeneratorCommand.SupportedFeatures then
  begin
    /// <summary>
    /// Checa se existe alguma sequence no banco, da qual não exista nos modelos
    /// para exclusão da mesma.
    /// </summary>
    for oSequenceTarget in ATargetDB.Sequences do
    begin
      if not AMasterDB.Sequences.ContainsKey(oSequenceTarget.Key) then
        ActionDropSequence(oSequenceTarget.Value);
    end;
    /// <summary>
    /// Checa se existe a sequence no banco, se não existir cria se existir.
    /// </summary>
    for oSequenceMaster in AMasterDB.Sequences do
    begin
      if not ATargetDB.Sequences.ContainsKey(oSequenceMaster.Key) then
        ActionCreateSequence(oSequenceMaster.Value);
    end;
  end;
end;

procedure TDatabaseFactory.ActionAlterColumn(AColumn: TColumnMIK);
begin
  FDDLCommands.Add(TDDLCommandAlterColumn.Create(AColumn));
end;

procedure TDatabaseFactory.ActionAlterDefaultValue(AColumn: TColumnMIK);
begin
  FDDLCommands.Add(TDDLCommandAlterDefaultValue.Create(AColumn));
end;

procedure TDatabaseFactory.ActionCreateCheck(ACheck: TCheckMIK);
begin
  FDDLCommands.Add(TDDLCommandCreateCheck.Create(ACheck));
end;

procedure TDatabaseFactory.ActionAlterCheck(ACheck: TCheckMIK);
begin
  FDDLCommands.Add(TDDLCommandAlterCheck.Create(ACheck));
end;

procedure TDatabaseFactory.ActionCreateColumn(AColumn: TColumnMIK);
begin
  FDDLCommands.Add(TDDLCommandCreateColumn.Create(AColumn));
end;

procedure TDatabaseFactory.ActionCreateForeignKey(AForeignKey: TForeignKeyMIK);
begin
  FDDLCommands.Add(TDDLCommandCreateForeignKey.Create(AForeignKey));
end;

procedure TDatabaseFactory.ActionCreateIndexe(AIndexe: TIndexeKeyMIK);
begin
  FDDLCommands.Add(TDDLCommandCreateIndexe.Create(AIndexe));
end;

procedure TDatabaseFactory.ActionCreatePrimaryKey(APrimaryKey: TPrimaryKeyMIK);
begin
  FDDLCommands.Add(TDDLCommandCreatePrimaryKey.Create(APrimaryKey));
end;

procedure TDatabaseFactory.ActionCreateSequence(ASequence: TSequenceMIK);
begin
  FDDLCommands.Add(TDDLCommandCreateSequence.Create(ASequence));
end;

procedure TDatabaseFactory.ActionCreateTable(ATable: TTableMIK);
begin
  FDDLCommands.Add(TDDLCommandCreateTable.Create(ATable));
end;

procedure TDatabaseFactory.ActionCreateView(AView: TViewMIK);
begin
  FDDLCommands.Add(TDDLCommandCreateView.Create(AView));
end;

procedure TDatabaseFactory.ActionDropCheck(ACheck: TCheckMIK);
begin
  FDDLCommands.Add(TDDLCommandDropCheck.Create(ACheck));
end;

procedure TDatabaseFactory.ActionDropColumn(AColumn: TColumnMIK);
begin
  FDDLCommands.Add(TDDLCommandDropColumn.Create(AColumn));
end;

procedure TDatabaseFactory.ActionDropDefaultValue(AColumn: TColumnMIK);
begin
  FDDLCommands.Add(TDDLCommandDropDefaultValue.Create(AColumn));
end;

procedure TDatabaseFactory.ActionDropForeignKey(AForeignKey: TForeignKeyMIK);
begin
  FDDLCommands.Add(TDDLCommandDropForeignKey.Create(AForeignKey));
end;

procedure TDatabaseFactory.ActionDropIndexe(AIndexe: TIndexeKeyMIK);
begin
  FDDLCommands.Add(TDDLCommandDropIndexe.Create(AIndexe));
end;

procedure TDatabaseFactory.ActionDropPrimaryKey(APrimaryKey: TPrimaryKeyMIK);
begin
  FDDLCommands.Add(TDDLCommandDropPrimaryKey.Create(APrimaryKey));
end;

procedure TDatabaseFactory.ActionDropSequence(ASequence: TSequenceMIK);
begin
  FDDLCommands.Add(TDDLCommandDropSequence.Create(ASequence));
end;

procedure TDatabaseFactory.ActionDropTable(ATable: TTableMIK);
begin
  FDDLCommands.Add(TDDLCommandDropTable.Create(ATable));
end;

procedure TDatabaseFactory.ActionDropView(AView: TViewMIK);
begin
  FDDLCommands.Add(TDDLCommandDropView.Create(AView));
end;

procedure TDatabaseFactory.ActionEnableForeignKeys(AEnable: Boolean);
begin
  FDDLCommands.Add(TDDLCommandEnableForeignKeys.Create(AEnable));
end;

procedure TDatabaseFactory.ActionEnableTriggers(AEnable: Boolean);
begin
  FDDLCommands.Add(TDDLCommandEnableTriggers.Create(AEnable));
end;

end.

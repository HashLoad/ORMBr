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

unit ormbr.metadata.model;

interface

uses
  SysUtils,
  ormbr.metadata.extract,
  ormbr.metadata.register,
  ormbr.database.mapping,
  ormbr.mapping.classes,
  ormbr.mapping.explorer,
  ormbr.factory.interfaces;

type
  TModelMetadata = class(TModelMetadataAbstract)
  public
    procedure GetCatalogs; override;
    procedure GetSchemas; override;
    procedure GetTables; override;
    procedure GetColumns(ATable: TTableMIK; AClass: TClass); override;
    procedure GetPrimaryKey(ATable: TTableMIK; AClass: TClass); override;
    procedure GetIndexeKeys(ATable: TTableMIK; AClass: TClass); override;
    procedure GetForeignKeys(ATable: TTableMIK; AClass: TClass); override;
    procedure GetChecks(ATable: TTableMIK; AClass: TClass); override;
    procedure GetSequences; override;
    procedure GetProcedures; override;
    procedure GetFunctions; override;
    procedure GetViews; override;
    procedure GetTriggers; override;
    procedure GetModelMetadata; override;
  end;

implementation

{ TModelMetadata }

procedure TModelMetadata.GetModelMetadata;
begin
  GetCatalogs;
end;

procedure TModelMetadata.GetCatalogs;
begin
  FCatalogMetadata.Name := '';
  GetSchemas;
end;

procedure TModelMetadata.GetSchemas;
begin
  FCatalogMetadata.Schema := '';
  GetSequences;
  GetTables;
end;

procedure TModelMetadata.GetTables;
var
  LClass: TClass;
  LTable: TTableMIK;
  LTableMap: TTableMapping;
begin
  for LClass in TMappingExplorer.GetInstance.Repository.List.Entitys do
  begin
    LTableMap := TMappingExplorer.GetInstance.GetMappingTable(LClass);
    if LTableMap <> nil then
    begin
      LTable := TTableMIK.Create(FCatalogMetadata);
      LTable.Name := LTableMap.Name;
      LTable.Description := LTableMap.Description;
      /// <summary>
      /// Extrair colunas
      /// </summary>
      GetColumns(LTable, LClass);
      /// <summary>
      /// Extrair Primary Key
      /// </summary>
      GetPrimaryKey(LTable, LClass);
      /// <summary>
      /// Extrair Foreign Keys
      /// </summary>
      GetForeignKeys(LTable, LClass);
      /// <summary>
      /// Extrair Indexes
      /// </summary>
      GetIndexeKeys(LTable, LClass);
      /// <summary>
      /// Extrair Indexes
      /// </summary>
      GetChecks(LTable, LClass);
      /// <summary>
      /// Adiciona na lista de tabelas extraidas
      /// </summary>
      FCatalogMetadata.Tables.Add(UpperCase(LTable.Name), LTable);
    end;
  end;
end;

procedure TModelMetadata.GetChecks(ATable: TTableMIK; AClass: TClass);
var
  LCheck: TCheckMIK;
  LCheckMapList: TCheckMappingList;
  LCheckMap: TCheckMapping;
begin
  LCheckMapList := TMappingExplorer.GetInstance.GetMappingCheck(AClass);
  if LCheckMapList <> nil then
  begin
    for LCheckMap in LCheckMapList do
    begin
      LCheck := TCheckMIK.Create(ATable);
      LCheck.Name := LCheckMap.Name;
      LCheck.Condition := LCheckMap.Condition;
      LCheck.Description := '';
      ATable.Checks.Add(UpperCase(LCheck.Name), LCheck);
    end;
  end;
end;

procedure TModelMetadata.GetColumns(ATable: TTableMIK; AClass: TClass);
var
  LColumn: TColumnMIK;
  LColumnMap: TColumnMapping;
  LColumnMapList: TColumnMappingList;
begin
  LColumnMapList := TMappingExplorer.GetInstance.GetMappingColumn(AClass);
  if LColumnMapList <> nil then
  begin
    for LColumnMap in LColumnMapList do
    begin
      LColumn := TColumnMIK.Create(ATable);
      LColumn.Name := LColumnMap.ColumnName;
      LColumn.Description := LColumnMap.Description;
      LColumn.Position := LColumnMap.FieldIndex;
      LColumn.NotNull := LColumnMap.IsNotNull;
      LColumn.DefaultValue := LColumnMap.DefaultValue;
      LColumn.Size := LColumnMap.Size;
      LColumn.Precision := LColumnMap.Precision;
      LColumn.Scale := LColumnMap.Scale;
      LColumn.FieldType := LColumnMap.FieldType;
      /// <summary>
      /// Resolve Field Type
      /// </summary>
      GetFieldTypeDefinition(LColumn);
      try
        ATable.Fields.Add(FormatFloat('000000', LColumn.Position), LColumn);
      except
        on E: Exception do
        begin
          raise Exception.Create('ORMBr Erro in GetColumns() : '  + sLineBreak +
                                 'Table  : [' + ATable.Name  + ']' + sLineBreak +
                                 'Column : [' + LColumn.Name + ']' + sLineBreak +
                                 'Message: [' + e.Message + ']');
        end;
      end;
    end;
  end;
end;

procedure TModelMetadata.GetForeignKeys(ATable: TTableMIK; AClass: TClass);
var
  LForeignKey: TForeignKeyMIK;
  LForeignKeyMapList: TForeignKeyMappingList;
  LForeignKeyMap: TForeignKeyMapping;

  procedure GetForeignKeyColumns(AForeignKey: TForeignKeyMIK);
  var
    LFromField: TColumnMIK;
    LToField: TColumnMIK;
    LFor: Integer;
  begin
    /// FromColumns
    for LFor := 0 to LForeignKeyMap.FromColumns.Count -1 do
    begin
      LFromField := TColumnMIK.Create(ATable);
      LFromField.Name := LForeignKeyMap.FromColumns[LFor];
      LFromField.Description := LForeignKeyMap.Description;
      LFromField.Position := LFor;
      AForeignKey.FromFields.Add(FormatFloat('000000', LFromField.Position), LFromField);
    end;
    /// ToColumns
    for LFor := 0 to LForeignKeyMap.ToColumns.Count -1 do
    begin
      LToField := TColumnMIK.Create(ATable);
      LToField.Name := LForeignKeyMap.ToColumns[LFor];
      LToField.Description := LForeignKeyMap.Description;
      LToField.Position := LFor;
      AForeignKey.ToFields.Add(FormatFloat('000000', LToField.Position), LToField);
    end;
  end;

begin
  LForeignKeyMapList := TMappingExplorer.GetInstance.GetMappingForeignKey(AClass);
  if LForeignKeyMapList <> nil then
  begin
    for LForeignKeyMap in LForeignKeyMapList do
    begin
      LForeignKey := TForeignKeyMIK.Create(ATable);
      LForeignKey.Name := LForeignKeyMap.Name;
      LForeignKey.FromTable := LForeignKeyMap.TableNameRef;
      LForeignKey.OnUpdate := LForeignKeyMap.RuleUpdate;
      LForeignKey.OnDelete := LForeignKeyMap.RuleDelete;
      LForeignKey.Description := LForeignKeyMap.Description;
      ATable.ForeignKeys.Add(UpperCase(LForeignKey.Name), LForeignKey);
      /// <summary>
      /// Estrai as columnas da indexe key
      /// </summary>
      GetForeignKeyColumns(LForeignKey);
    end;
  end;
end;

procedure TModelMetadata.GetFunctions;
begin

end;

procedure TModelMetadata.GetPrimaryKey(ATable: TTableMIK; AClass: TClass);
var
  LPrimaryKeyMap: TPrimaryKeyMapping;

  procedure GetPrimaryKeyColumns(APrimaryKey: TPrimaryKeyMIK);
  var
    LColumn: TColumnMIK;
    LFor: Integer;
  begin
    for LFor := 0 to LPrimaryKeyMap.Columns.Count -1 do
    begin
      LColumn := TColumnMIK.Create(ATable);
      LColumn.Name := LPrimaryKeyMap.Columns[LFor];
      LColumn.Description := LPrimaryKeyMap.Description;
      LColumn.SortingOrder := LPrimaryKeyMap.SortingOrder;
      LColumn.AutoIncrement := LPrimaryKeyMap.AutoIncrement;
      LColumn.Position := LFor;
      APrimaryKey.Fields.Add(FormatFloat('000000', LColumn.Position), LColumn);
    end;
  end;

begin
  LPrimaryKeyMap := TMappingExplorer.GetInstance.GetMappingPrimaryKey(AClass);
  if LPrimaryKeyMap <> nil then
  begin
    ATable.PrimaryKey.Name := Format('PK_%s', [ATable.Name]);
    ATable.PrimaryKey.Description := LPrimaryKeyMap.Description;
    ATable.PrimaryKey.AutoIncrement := LPrimaryKeyMap.AutoIncrement;
    /// <summary>
    /// Estrai as columnas da primary key
    /// </summary>
    GetPrimaryKeyColumns(ATable.PrimaryKey);
  end;
end;

procedure TModelMetadata.GetProcedures;
begin

end;

procedure TModelMetadata.GetSequences;
var
  LClass: TClass;
  LSequence: TSequenceMIK;
  LSequenceMap: TSequenceMapping;
begin
  for LClass in TMappingExplorer.GetInstance.Repository.List.Entitys do
  begin
    LSequenceMap := TMappingExplorer.GetInstance.GetMappingSequence(LClass);
    if LSequenceMap <> nil then
    begin
      LSequence := TSequenceMIK.Create(FCatalogMetadata);
      LSequence.TableName := LSequenceMap.TableName;
      LSequence.Name := LSequenceMap.Name;
      LSequence.Description := LSequenceMap.Description;
      LSequence.InitialValue := LSequenceMap.Initial;
      LSequence.Increment := LSequenceMap.Increment;
      if FConnection.GetDriverName = dnMySQL then
        FCatalogMetadata.Sequences.Add(UpperCase(LSequence.TableName), LSequence)
      else
        FCatalogMetadata.Sequences.Add(UpperCase(LSequence.Name), LSequence);
    end;
  end;
end;

procedure TModelMetadata.GetTriggers;
begin

end;

procedure TModelMetadata.GetIndexeKeys(ATable: TTableMIK; AClass: TClass);
var
  LIndexeKey: TIndexeKeyMIK;
  LIndexeKeyMapList: TIndexeMappingList;
  LIndexeKeyMap: TIndexeMapping;

  procedure GetIndexeKeyColumns(AIndexeKey: TIndexeKeyMIK);
  var
    LColumn: TColumnMIK;
    LFor: Integer;
  begin
    for LFor := 0 to LIndexeKeyMap.Columns.Count -1 do
    begin
      LColumn := TColumnMIK.Create(ATable);
      LColumn.Name := LIndexeKeyMap.Columns[LFor];
      LColumn.Description := LIndexeKeyMap.Description;
      LColumn.SortingOrder := LIndexeKeyMap.SortingOrder;
      LColumn.Position := LFor;
      AIndexeKey.Fields.Add(FormatFloat('000000', LColumn.Position), LColumn);
    end;
  end;

begin
  LIndexeKeyMapList := TMappingExplorer.GetInstance.GetMappingIndexe(AClass);
  if LIndexeKeyMapList <> nil then
  begin
    for LIndexeKeyMap in LIndexeKeyMapList do
    begin
      LIndexeKey := TIndexeKeyMIK.Create(ATable);
      LIndexeKey.Name := LIndexeKeyMap.Name;
      LIndexeKey.Unique := LIndexeKeyMap.Unique;
      LIndexeKey.Description := '';
      ATable.IndexeKeys.Add(UpperCase(LIndexeKey.Name), LIndexeKey);
      /// <summary>
      /// Estrai as columnas da indexe key
      /// </summary>
      GetIndexeKeyColumns(LIndexeKey);
    end;
  end;
end;

procedure TModelMetadata.GetViews;
begin

end;

end.

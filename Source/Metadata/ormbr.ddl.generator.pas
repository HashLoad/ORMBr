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
  @created(12 Out 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.ddl.generator;

interface

uses
  SysUtils,
  Generics.Collections,
  ormbr.ddl.interfaces,
  ormbr.factory.interfaces,
  ormbr.database.mapping,
  ormbr.types.mapping;

type
  TDDLSQLGeneratorAbstract = class abstract(TInterfacedObject, IDDLGeneratorCommand)
  protected
    FConnection: IDBConnection;
  public
    function GenerateCreateTable(ATable: TTableMIK): string; virtual; abstract;
    function GenerateCreateColumn(AColumn: TColumnMIK): string; virtual; abstract;
    function GenerateCreatePrimaryKey(APrimaryKey: TPrimaryKeyMIK): string; virtual; abstract;
    function GenerateCreateForeignKey(AForeignKey: TForeignKeyMIK): string; virtual; abstract;
    function GenerateCreateSequence(ASequence: TSequenceMIK): string; virtual; abstract;
    function GenerateCreateIndexe(AIndexe: TIndexeKeyMIK): string; virtual; abstract;
    function GenerateCreateCheck(ACheck: TCheckMIK): string; virtual; abstract;
    function GenerateCreateView(AView: TViewMIK): string; virtual; abstract;
    function GenerateCreateTrigger(ATrigger: TTriggerMIK): string; virtual; abstract;
    function GenerateAlterColumn(AColumn: TColumnMIK): string; virtual; abstract;
    function GenerateAlterDefaultValue(AColumn: TColumnMIK): string; virtual; abstract;
    function GenerateAlterCheck(ACheck: TCheckMIK): string; virtual; abstract;
    function GenerateAddPrimaryKey(APrimaryKey: TPrimaryKeyMIK): string; virtual; abstract;
    function GenerateDropTable(ATable: TTableMIK): string; virtual; abstract;
    function GenerateDropPrimaryKey(APrimaryKey: TPrimaryKeyMIK): string; virtual; abstract;
    function GenerateDropForeignKey(AForeignKey: TForeignKeyMIK): string; virtual; abstract;
    function GenerateDropSequence(ASequence: TSequenceMIK): string; virtual; abstract;
    function GenerateDropIndexe(AIndexe: TIndexeKeyMIK): string; virtual; abstract;
    function GenerateDropCheck(ACheck: TCheckMIK): string; virtual; abstract;
    function GenerateDropColumn(AColumn: TColumnMIK): string; virtual; abstract;
    function GenerateDropDefaultValue(AColumn: TColumnMIK): string; virtual; abstract;
    function GenerateDropView(AView: TViewMIK): string; virtual; abstract;
    function GenerateDropTrigger(ATrigger: TTriggerMIK): string; virtual; abstract;
    function GenerateEnableForeignKeys(AEnable: Boolean): string; virtual; abstract;
    function GenerateEnableTriggers(AEnable: Boolean): string; virtual; abstract;
    /// <summary>
    /// Propriedade para identificar os recursos de diferentes banco de dados
    /// usando o mesmo modelo.
    /// </summary>
    function GetSupportedFeatures: TSupportedFeatures; virtual; abstract;
    property SupportedFeatures: TSupportedFeatures read GetSupportedFeatures;
  end;

  TDDLSQLGenerator = class(TDDLSQLGeneratorAbstract)
  protected
    function GetRuleDeleteActionDefinition(ARuleAction: TRuleAction): string;
    function GetRuleUpdateActionDefinition(ARuleAction: TRuleAction): string;
    function GetPrimaryKeyColumnsDefinition(APrimaryKey: TPrimaryKeyMIK): string;
    function GetForeignKeyFromColumnsDefinition(AForeignKey: TForeignKeyMIK): string;
    function GetForeignKeyToColumnsDefinition(AForeignKey: TForeignKeyMIK): string;
    function GetIndexeKeyColumnsDefinition(AIndexeKey: TIndexeKeyMIK): string;
    function GetUniqueColumnDefinition(AUnique: Boolean): string;
    function GetFieldTypeDefinition(AColumn: TColumnMIK): string;
    function GetFieldNotNullDefinition(AColumn: TColumnMIK): string;
    function GetCreateFieldDefaultDefinition(AColumn: TColumnMIK): string;
    function GetAlterFieldDefaultDefinition(AColumn: TColumnMIK): string;
    function BuilderCreateFieldDefinition(AColumn: TColumnMIK): string; virtual;
    function BuilderAlterFieldDefinition(AColumn: TColumnMIK): string; virtual;
    function BuilderPrimayKeyDefinition(ATable: TTableMIK): string; virtual;
    function BuilderIndexeDefinition(ATable: TTableMIK): string; virtual;
    function BuilderForeignKeyDefinition(ATable: TTableMIK): string; virtual;
    function BuilderCheckDefinition(ATable: TTableMIK): string; virtual;
  public
    function GenerateCreateTable(ATable: TTableMIK): string; override;
    function GenerateCreateColumn(AColumn: TColumnMIK): string; override;
    function GenerateCreatePrimaryKey(APrimaryKey: TPrimaryKeyMIK): string; override;
    function GenerateCreateForeignKey(AForeignKey: TForeignKeyMIK): string; override;
    function GenerateCreateView(AView: TViewMIK): string; override;
    function GenerateCreateTrigger(ATrigger: TTriggerMIK): string; override;
    function GenerateCreateSequence(ASequence: TSequenceMIK): string; override;
    function GenerateCreateIndexe(AIndexe: TIndexeKeyMIK): string; override;
    function GenerateCreateCheck(ACheck: TCheckMIK): string; override;
    function GenerateAlterColumn(AColumn: TColumnMIK): string; override;
    function GenerateAlterCheck(ACheck: TCheckMIK): string; override;
    function GenerateAddPrimaryKey(APrimaryKey: TPrimaryKeyMIK): string; override;
    function GenerateDropTable(ATable: TTableMIK): string; override;
    function GenerateDropColumn(AColumn: TColumnMIK): string; override;
    function GenerateDropPrimaryKey(APrimaryKey: TPrimaryKeyMIK): string; override;
    function GenerateDropForeignKey(AForeignKey: TForeignKeyMIK): string; override;
    function GenerateDropIndexe(AIndexe: TIndexeKeyMIK): string; override;
    function GenerateDropCheck(ACheck: TCheckMIK): string; override;
    function GenerateDropView(AView: TViewMIK): string; override;
    function GenerateDropTrigger(ATrigger: TTriggerMIK): string; override;
    function GenerateDropSequence(ASequence: TSequenceMIK): string; override;
    /// <summary>
    /// Propriedade para identificar os recursos de diferentes banco de dados
    /// usando o mesmo modelo.
    /// </summary>
    function GetSupportedFeatures: TSupportedFeatures; override;
    property SupportedFeatures: TSupportedFeatures read GetSupportedFeatures;
  end;

implementation

uses
  StrUtils;

{ TDDLSQLGenerator }

function TDDLSQLGenerator.GenerateCreateTable(ATable: TTableMIK): string;
begin
  Result := 'CREATE TABLE %s (';
end;

function TDDLSQLGenerator.GenerateCreateTrigger(ATrigger: TTriggerMIK): string;
begin
  Result := 'CREATE TRIGGER %s AS %s;';
  Result := Format(Result, [ATrigger.Name, ATrigger.Script]);
end;

function TDDLSQLGenerator.GenerateCreateView(AView: TViewMIK): string;
begin
  Result := 'CREATE VIEW %s AS %s;';
  Result := Format(Result, [AView.Name, AView.Script]);
end;

function TDDLSQLGenerator.GenerateDropTable(ATable: TTableMIK): string;
begin
  Result := 'DROP TABLE %s;';
  if ATable.Database.Schema <> '' then
    Result := Format(Result, [ATable.Database.Schema + '.' + ATable.Name])
  else
    Result := Format(Result, [ATable.Name]);
end;

function TDDLSQLGenerator.GenerateDropTrigger(ATrigger: TTriggerMIK): string;
begin
  Result := 'DROP TRIGGER %s;';
  Result := Format(Result, [ATrigger.Name]);
end;

function TDDLSQLGenerator.GenerateDropView(AView: TViewMIK): string;
begin
  Result := 'DROP VIEW %s;';
  Result := Format(Result, [AView.Name]);
end;

function TDDLSQLGenerator.GenerateAddPrimaryKey(APrimaryKey: TPrimaryKeyMIK): string;
begin
  Result := 'ALTER TABLE %s ADD PRIMARY KEY (%s);';
  Result := Format(Result, [APrimaryKey.Table.Name,
                            GetPrimaryKeyColumnsDefinition(APrimaryKey)]);
end;

function TDDLSQLGenerator.GenerateAlterColumn(AColumn: TColumnMIK): string;
begin
  Result := 'ALTER TABLE %s ALTER COLUMN %s;';
  Result := Format(Result, [AColumn.Table.Name, BuilderAlterFieldDefinition(AColumn)]);
end;

function TDDLSQLGenerator.GenerateCreateCheck(ACheck: TCheckMIK): string;
begin
  Result := 'CONSTRAINT %s CHECK (%s)';
  Result := Format(Result, [ACheck.Name, ACheck.Condition]);
end;

function TDDLSQLGenerator.GenerateAlterCheck(ACheck: TCheckMIK): string;
begin
  Result := 'ALTER TABLE %s ADD CONSTRAINT %s CHECK (%s);';
  Result := Format(Result, [ACheck.Table.Name,  ACheck.Name, ACheck.Condition]);
end;

function TDDLSQLGenerator.GenerateCreateColumn(AColumn: TColumnMIK): string;
begin
  Result := 'ALTER TABLE %s ADD %s;';
  Result := Format(Result, [AColumn.Table.Name, BuilderCreateFieldDefinition(AColumn)]);
end;

function TDDLSQLGenerator.GenerateCreateForeignKey(AForeignKey: TForeignKeyMIK): string;
begin
  Result := 'ALTER TABLE %s ADD CONSTRAINT %s FOREIGN KEY (%s) REFERENCES %s(%s) %s %s';
  Result := Format(Result, [AForeignKey.Table.Name,
                            AForeignKey.Name,
                            GetForeignKeyFromColumnsDefinition(AForeignKey),
                            AForeignKey.FromTable,
                            GetForeignKeyToColumnsDefinition(AForeignKey),
                            GetRuleDeleteActionDefinition(AForeignKey.OnDelete),
                            GetRuleUpdateActionDefinition(AForeignKey.OnUpdate)]);
  Result := Trim(Result) + ';';
end;

function TDDLSQLGenerator.GenerateCreateIndexe(AIndexe: TIndexeKeyMIK): string;
begin
  Result := 'CREATE %s INDEX %s ON %s (%s);';
  Result := Format(Result, [GetUniqueColumnDefinition(AIndexe.Unique),
                            AIndexe.Name,
                            AIndexe.Table.Name,
                            GetIndexeKeyColumnsDefinition(AIndexe)]);
end;

function TDDLSQLGenerator.GenerateCreatePrimaryKey(APrimaryKey: TPrimaryKeyMIK): string;
begin
  Result := 'CONSTRAINT %s PRIMARY KEY (%s)';
  Result := Format(Result, [APrimaryKey.Name,
                            GetPrimaryKeyColumnsDefinition(APrimaryKey)]);
end;

function TDDLSQLGenerator.GenerateCreateSequence(ASequence: TSequenceMIK): string;
begin
  Result := '';
end;

function TDDLSQLGenerator.GenerateDropColumn(AColumn: TColumnMIK): string;
begin
  Result := 'ALTER TABLE %s DROP COLUMN %s;';
  Result := Format(Result, [AColumn.Table.Name, AColumn.Name]);
end;

function TDDLSQLGenerator.GenerateDropForeignKey(AForeignKey: TForeignKeyMIK): string;
begin
  Result := 'ALTER TABLE %s DROP CONSTRAINT %s;';
  Result := Format(Result, [AForeignKey.Table.Name, AForeignKey.Name]);
end;

function TDDLSQLGenerator.GenerateDropIndexe(AIndexe: TIndexeKeyMIK): string;
begin
  Result := 'DROP INDEX %s ON %s;';
  Result := Format(Result, [AIndexe.Name, AIndexe.Table.Name]);
end;

function TDDLSQLGenerator.GenerateDropCheck(ACheck: TCheckMIK): string;
begin
  Result := 'ALTER TABLE %s DROP CONSTRAINT %s;';
  Result := Format(Result, [ACheck.Table.Name, ACheck.Name]);
end;

function TDDLSQLGenerator.GenerateDropPrimaryKey(APrimaryKey: TPrimaryKeyMIK): string;
begin
  Result := 'ALTER TABLE %s DROP CONSTRAINT %s;';
  Result := Format(Result, [APrimaryKey.Table.Name, APrimaryKey.Name]);
end;

function TDDLSQLGenerator.GenerateDropSequence(ASequence: TSequenceMIK): string;
begin
  Result := 'DROP SEQUENCE %s;';
  Result := Format(Result, [ASequence.Name]);
end;

function TDDLSQLGenerator.GetAlterFieldDefaultDefinition(AColumn: TColumnMIK): string;
begin
  Result := IfThen(Length(AColumn.DefaultValue) > 0, AColumn.DefaultValue, '');
end;

function TDDLSQLGenerator.GetCreateFieldDefaultDefinition(AColumn: TColumnMIK): string;
begin
  Result := IfThen(Length(AColumn.DefaultValue) > 0, ' DEFAULT ' + AColumn.DefaultValue, '');
end;

function TDDLSQLGenerator.BuilderAlterFieldDefinition(AColumn: TColumnMIK): string;
begin
  Result := AColumn.Name + ' ' +
            GetFieldTypeDefinition(AColumn)    +
//            GetAlterFieldDefaultDefinition(AColumn) +
            GetFieldNotNullDefinition(AColumn) ;
end;

function TDDLSQLGenerator.BuilderCheckDefinition(ATable: TTableMIK): string;
var
  oCheck: TPair<string,TCheckMIK>;
begin
  Result := '';
  for oCheck in ATable.Checks do
  begin
    Result := Result + sLineBreak;
    Result := Result + '  ' + GenerateCreateCheck(oCheck.Value);
  end;
end;

function TDDLSQLGenerator.BuilderCreateFieldDefinition(AColumn: TColumnMIK): string;
begin
  Result := AColumn.Name + ' ' +
            GetFieldTypeDefinition(AColumn)    +
            GetCreateFieldDefaultDefinition(AColumn) +
            GetFieldNotNullDefinition(AColumn) ;
end;

function TDDLSQLGenerator.BuilderForeignKeyDefinition(ATable: TTableMIK): string;
var
  oForeignKey: TPair<string,TForeignKeyMIK>;
begin
  Result := '';
  for oForeignKey in ATable.ForeignKeys do
  begin
    Result := Result + sLineBreak;
    Result := Result + '  ' + GenerateCreateForeignKey(oForeignKey.Value);
  end;
end;

function TDDLSQLGenerator.GetFieldNotNullDefinition(AColumn: TColumnMIK): string;
begin
  Result := ifThen(AColumn.NotNull, ' NOT NULL', '');
end;

function TDDLSQLGenerator.GetFieldTypeDefinition(AColumn: TColumnMIK): string;
var
  LResult: string;
begin
  LResult := AColumn.TypeName;
  LResult := StringReplace(LResult, '%l', IntToStr(AColumn.Size), [rfIgnoreCase]);
  LResult := StringReplace(LResult, '%p', IntToStr(AColumn.Precision), [rfIgnoreCase]);
  LResult := StringReplace(LResult, '%s', IntToStr(AColumn.Scale), [rfIgnoreCase]);
  Result  := ' ' + LResult;
end;

function TDDLSQLGenerator.BuilderIndexeDefinition(ATable: TTableMIK): string;
var
  oIndexe: TPair<string,TIndexeKeyMIK>;
begin
  Result := '';
  for oIndexe in ATable.IndexeKeys do
  begin
    Result := Result + sLineBreak;
    Result := Result + GenerateCreateIndexe(oIndexe.Value);
  end;
end;

function TDDLSQLGenerator.BuilderPrimayKeyDefinition(ATable: TTableMIK): string;
begin
  Result := '  ' + GenerateCreatePrimaryKey(ATable.PrimaryKey);
end;

function TDDLSQLGenerator.GetRuleDeleteActionDefinition(ARuleAction: TRuleAction): string;
begin
  Result := '';
  if      ARuleAction in [Cascade]    then Result := 'ON DELETE CASCADE'
  else if ARuleAction in [SetNull]    then Result := 'ON DELETE SET NULL'
  else if ARuleAction in [SetDefault] then Result := 'ON DELETE SET DEFAULT';
end;

function TDDLSQLGenerator.GetRuleUpdateActionDefinition(ARuleAction: TRuleAction): string;
begin
  Result := '';
  if      ARuleAction in [Cascade]    then Result := 'ON UPDATE CASCADE'
  else if ARuleAction in [SetNull]    then Result := 'ON UPDATE SET NULL'
  else if ARuleAction in [SetDefault] then Result := 'ON UPDATE SET DEFAULT';
end;

function TDDLSQLGenerator.GetSupportedFeatures: TSupportedFeatures;
begin
  Result := [TSupportedFeature.Sequences,
             TSupportedFeature.ForeignKeys,
             TSupportedFeature.Checks,
             TSupportedFeature.Views,
             TSupportedFeature.Triggers];
end;

function TDDLSQLGenerator.GetUniqueColumnDefinition(AUnique: Boolean): string;
begin
  Result := ifThen(AUnique, 'UNIQUE', '');
end;

function TDDLSQLGenerator.GetForeignKeyFromColumnsDefinition(AForeignKey: TForeignKeyMIK): string;
var
  oColumn: TPair<string,TColumnMIK>;
begin
  for oColumn in AForeignKey.FromFieldsSort do
    Result := Result + oColumn.Value.Name + ', ';
  Result := Trim(Result);
  Delete(Result, Length(Result), 1);
end;

function TDDLSQLGenerator.GetForeignKeyToColumnsDefinition(AForeignKey: TForeignKeyMIK): string;
var
  oColumn: TPair<string,TColumnMIK>;
begin
  for oColumn in AForeignKey.ToFieldsSort do
    Result := Result + oColumn.Value.Name + ', ';
  Result := Trim(Result);
  Delete(Result, Length(Result), 1);
end;

function TDDLSQLGenerator.GetIndexeKeyColumnsDefinition(AIndexeKey: TIndexeKeyMIK): string;
var
  oColumn: TPair<string,TColumnMIK>;
begin
  for oColumn in AIndexeKey.FieldsSort do
    Result := Result + oColumn.Value.Name + ', ';
  Result := Trim(Result);
  Delete(Result, Length(Result), 1);
end;

function TDDLSQLGenerator.GetPrimaryKeyColumnsDefinition(APrimaryKey: TPrimaryKeyMIK): string;
var
  oColumn: TPair<string,TColumnMIK>;
begin
  for oColumn in APrimaryKey.FieldsSort do
    Result := Result + oColumn.Value.Name + ', ';
  Result := Trim(Result);
  Delete(Result, Length(Result), 1);
end;

end.

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

unit ormbr.ddl.generator.firebird;

interface

uses
  SysUtils,
  StrUtils,
  Generics.Collections,
  ormbr.ddl.register,
  ormbr.ddl.generator,
  ormbr.factory.interfaces,
  ormbr.database.mapping;

type
  TDDLSQLGeneratorFirebird = class(TDDLSQLGenerator)
  protected
  public
    function GenerateCreateTable(ATable: TTableMIK): string; override;
    function GenerateCreateSequence(ASequence: TSequenceMIK): string; override;
    function GenerateCreateForeignKey(AForeignKey: TForeignKeyMIK): string; override;
    function GenerateDropSequence(ASequence: TSequenceMIK): string; override;
    function GenerateDropIndexe(AIndexe: TIndexeKeyMIK): string; override;
    function GenerateDropColumn(AColumn: TColumnMIK): string; override;
    function GenerateEnableForeignKeys(AEnable: Boolean): string; override;
    function GenerateEnableTriggers(AEnable: Boolean): string; override;
    function GenerateAlterColumn(AColumn: TColumnMIK): string; override;
    function GenerateAlterDefaultValue(AColumn: TColumnMIK): string; override;
    function GenerateDropDefaultValue(AColumn: TColumnMIK): string; override;
    function GenerateCreateView(AView: TViewMIK): string; override;
  end;

implementation

{ TDDLSQLGeneratorFirebird }

function TDDLSQLGeneratorFirebird.GenerateAlterDefaultValue(AColumn: TColumnMIK): string;
begin
  Result := 'ALTER TABLE %s ALTER COLUMN %s SET DEFAULT %s;';
  Result := Format(Result, [AColumn.Table.Name,
                            AColumn.Name,
                            GetAlterFieldDefaultDefinition(AColumn)]);
end;

function TDDLSQLGeneratorFirebird.GenerateCreateForeignKey(AForeignKey: TForeignKeyMIK): string;
begin
  Result := 'ALTER TABLE %s ADD CONSTRAINT %s FOREIGN KEY (%s) REFERENCES %s(%s) %s %s;';

  Result := Format(Result, [AForeignKey.Table.Name,
                            AForeignKey.Name,
                            GetForeignKeyFromColumnsDefinition(AForeignKey),
                            AForeignKey.FromTable,
                            GetForeignKeyToColumnsDefinition(AForeignKey),
                            GetRuleDeleteActionDefinition(AForeignKey.OnDelete),
                            GetRuleUpdateActionDefinition(AForeignKey.OnUpdate)]);
end;

function TDDLSQLGeneratorFirebird.GenerateCreateSequence(ASequence: TSequenceMIK): string;
begin
  Result := 'CREATE GENERATOR %s;';
  Result := Format(Result, [ASequence.Name]);
end;

function TDDLSQLGeneratorFirebird.GenerateCreateTable(ATable: TTableMIK): string;
var
  oSQL: TStringBuilder;
  oColumn: TPair<string,TColumnMIK>;
begin
  oSQL := TStringBuilder.Create;
  Result := inherited GenerateCreateTable(ATable);
  try
    if ATable.Database.Schema <> '' then
      oSQL.Append(Format(Result, [ATable.Database.Schema + '.' + ATable.Name]))
    else
      oSQL.Append(Format(Result, [ATable.Name]));
    /// <summary>
    /// Add Colunas
    /// </summary>
    for oColumn in ATable.FieldsSort do
    begin
      oSQL.AppendLine;
      oSQL.Append(BuilderCreateFieldDefinition(oColumn.Value));
      oSQL.Append(',');
    end;
    /// <summary>
    /// Add PrimariKey
    /// </summary>
    if ATable.PrimaryKey.Fields.Count > 0 then
    begin
      oSQL.AppendLine;
      oSQL.Append(BuilderPrimayKeyDefinition(ATable));
    end;
    /// <summary>
    /// Add ForeignKey
    /// </summary>
//    if ATable.ForeignKeys.Count > 0 then
//    begin
//      oSQL.Append(',');
//      oSQL.Append(BuilderForeignKeyDefinition(ATable));
//    end;
    /// <summary>
    /// Add Checks
    /// </summary>
    if ATable.Checks.Count > 0 then
    begin
      oSQL.Append(',');
      oSQL.Append(BuilderCheckDefinition(ATable));
    end;
    oSQL.AppendLine;
    oSQL.Append(');');
    /// <summary>
    /// Add Indexe
    /// </summary>
    if ATable.IndexeKeys.Count > 0 then
      oSQL.Append(BuilderIndexeDefinition(ATable));
    oSQL.AppendLine;
    Result := oSQL.ToString;
  finally
    oSQL.Free;
  end;
end;

function TDDLSQLGeneratorFirebird.GenerateCreateView(AView: TViewMIK): string;
begin
end;

function TDDLSQLGeneratorFirebird.GenerateDropColumn(AColumn: TColumnMIK): string;
begin
  Result := 'ALTER TABLE %s DROP %s;';
  Result := Format(Result, [AColumn.Table.Name, AColumn.Name]);
end;

function TDDLSQLGeneratorFirebird.GenerateDropDefaultValue(AColumn: TColumnMIK): string;
begin
  Result := 'ALTER TABLE %s ALTER COLUMN %s DROP DEFAULT;';
  Result := Format(Result, [aColumn.Table.Name, AColumn.Name]);
end;

function TDDLSQLGeneratorFirebird.GenerateDropIndexe(AIndexe: TIndexeKeyMIK): string;
begin
  Result := 'DROP INDEX %s ;';
  Result := Format(Result, [AIndexe.Name]);
end;

function TDDLSQLGeneratorFirebird.GenerateDropSequence(ASequence: TSequenceMIK): string;
begin
  Result := 'DROP GENERATOR %s;';
  Result := Format(Result, [ASequence.Name]);
end;

function TDDLSQLGeneratorFirebird.GenerateAlterColumn(AColumn: TColumnMIK): string;
begin
  Result := 'ALTER TABLE %s ALTER COLUMN %s TYPE %s;';
  Result := Format(Result, [AColumn.Table.Name,
                            AColumn.Name,
                            BuilderAlterFieldDefinition(AColumn)]);
end;

function TDDLSQLGeneratorFirebird.GenerateEnableForeignKeys(AEnable: Boolean): string;
begin
  if AEnable then
    Result := ''
  else
    Result := '';
end;

function TDDLSQLGeneratorFirebird.GenerateEnableTriggers(AEnable: Boolean): string;
begin
  if AEnable then
    Result := 'UPDATE RDB$TRIGGERS SET RDB$TRIGGER_INACTIVE = 0 ' +
              'WHERE RDB$TRIGGER_SOURCE IS NOT NULL AND ((RDB$SYSTEM_FLAG = 0) OR (RDB$SYSTEM_FLAG IS NULL));'
  else
    Result := 'UPDATE RDB$TRIGGERS SET RDB$TRIGGER_INACTIVE = 1 ' +
              'WHERE RDB$TRIGGER_SOURCE IS NOT NULL AND ((RDB$SYSTEM_FLAG = 0) OR (RDB$SYSTEM_FLAG IS NULL));';
end;

initialization
  TSQLDriverRegister.GetInstance.RegisterDriver(dnFirebird, TDDLSQLGeneratorFirebird.Create);

end.

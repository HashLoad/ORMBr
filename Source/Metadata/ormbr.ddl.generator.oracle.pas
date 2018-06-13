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

unit ormbr.ddl.generator.oracle;

interface

uses
  SysUtils,
  StrUtils,
  Variants,
  Generics.Collections,
  ormbr.ddl.register,
  ormbr.ddl.generator,
  ormbr.factory.interfaces,
  ormbr.database.mapping;

type
  TDDLSQLGeneratorMySQL = class(TDDLSQLGenerator)
  protected
  public
    function GenerateCreateTable(ATable: TTableMIK): string; override;
    function GenerateCreateSequence(ASequence: TSequenceMIK): string; override;
    function GenerateCreateForeignKey(AForeignKey: TForeignKeyMIK): string; override;
    function GenerateEnableForeignKeys(AEnable: Boolean): string; override;
    function GenerateEnableTriggers(AEnable: Boolean): string; override;
    function GenerateAlterColumn(AColumn: TColumnMIK): string; override;
    function GenerateDropPrimaryKey(APrimaryKey: TPrimaryKeyMIK): string; override;
    function GenerateDropIndexe(AIndexe: TIndexeKeyMIK): string; override;
  end;

implementation

{ TDDLSQLGeneratorMySQL }

function TDDLSQLGeneratorMySQL.GenerateAlterColumn(AColumn: TColumnMIK): string;
begin
  Result := 'ALTER TABLE %s MODIFY COLUMN %s;';
  Result := Format(Result, [AColumn.Table.Name, BuilderAlterFieldDefinition(AColumn)]);
end;

function TDDLSQLGeneratorMySQL.GenerateCreateForeignKey(AForeignKey: TForeignKeyMIK): string;
begin
  Result := 'ALTER TABLE %s ADD CONSTRAINT %s FOREIGN KEY (%s) REFERENCES %s(%s) %s ENABLE VALIDATE;';
  Result := Format(Result, [AForeignKey.Table.Name,
                            AForeignKey.Name,
                            GetForeignKeyFromColumnsDefinition(AForeignKey),
                            AForeignKey.FromTable,
                            GetForeignKeyToColumnsDefinition(AForeignKey),
                            GetRuleDeleteActionDefinition(AForeignKey.OnDelete)]);
end;

function TDDLSQLGeneratorMySQL.GenerateCreateSequence(
  ASequence: TSequenceMIK): string;
begin
  Result := 'CREATE SEQUENCE %s MINVALUE 1 START WITH %s INCREMENT BY %s NOCACHE;'; // MAXVALUE ????? CACHE ??
  Result := Format(Result, [ASequence.Name,
                            IntToStr(ASequence.InitialValue +1),
                            IntToStr(ASequence.Increment)]);
end;

function TDDLSQLGeneratorMySQL.GenerateCreateTable(ATable: TTableMIK): string;
var
  oSQL: TStringBuilder;
  oColumn: TPair<string, TColumnMIK>;
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
      oSQL.Append('  ' + BuilderCreateFieldDefinition(oColumn.Value));
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

function TDDLSQLGeneratorMySQL.GenerateDropIndexe(AIndexe: TIndexeKeyMIK): string;
begin
  Result := 'ALTER TABLE %s DROP INDEX %s;';
  Result := Format(Result, [AIndexe.Table.Name, AIndexe.Name]);
end;

function TDDLSQLGeneratorMySQL.GenerateDropPrimaryKey(APrimaryKey: TPrimaryKeyMIK): string;
begin
  Result := 'ALTER TABLE %s DROP PRIMARY KEY;';
  Result := Format(Result, [APrimaryKey.Table.Name]);
end;

function TDDLSQLGeneratorMySQL.GenerateEnableForeignKeys(AEnable: Boolean): string;
begin
  if AEnable then
    Result := ''
  else
    Result := '';
end;

function TDDLSQLGeneratorMySQL.GenerateEnableTriggers(AEnable: Boolean): string;
begin
  if AEnable then
    Result := ''
  else
    Result := '';
end;

initialization
  TSQLDriverRegister.GetInstance.RegisterDriver(dnOracle, TDDLSQLGeneratorMySQL.Create);

end.

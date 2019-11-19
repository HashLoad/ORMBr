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

unit ormbr.ddl.generator.postgresql;

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
  TDDLSQLGeneratorPostgreSQL = class(TDDLSQLGenerator)
  protected
  public
    function GenerateCreateTable(ATable: TTableMIK): string; override;
    function GenerateCreateSequence(ASequence: TSequenceMIK): string; override;
    function GenerateEnableForeignKeys(AEnable: Boolean): string; override;
    function GenerateEnableTriggers(AEnable: Boolean): string; override;
    function GenerateAlterColumn(AColumn: TColumnMIK): string; override;
    function GenerateDropPrimaryKey(APrimaryKey: TPrimaryKeyMIK): string; override;
    function GenerateDropIndexe(AIndexe: TIndexeKeyMIK): string; override;
  end;

implementation

{ TDDLSQLGeneratorPostgreSQL }

function TDDLSQLGeneratorPostgreSQL.GenerateAlterColumn(AColumn: TColumnMIK): string;
var
  LSchemaName: string;
  LBuilder: TStringBuilder;
begin
  LSchemaName := AColumn.Table.Database.Schema;
  Result := Format('ALTER TABLE %s.%s ALTER COLUMN %s', [LSchemaName,
                                                         AColumn.Table.Name,
                                                         AColumn.Name]);
  LBuilder := TStringBuilder.Create;
  try
    LBuilder.Append(Result + Format(' TYPE %s;', [GetFieldTypeDefinition(AColumn)]));

    if Length(AColumn.DefaultValue) > 0 then
      LBuilder.Append(Result + Format(' SET DEFAULT %s;', [AColumn.DefaultValue]))
    else
      LBuilder.Append(Result + ' DROP DEFAULT;');

    if AColumn.NotNull then
      LBuilder.Append(Result + ' SET NOT NULL;')
    else
      LBuilder.Append(Result + ' DROP NOT NULL;');

    LBuilder.Append(Format('COMMENT ON COLUMN %s.%s.%s IS %s', [LSchemaName,
                                                                AColumn.Table.Name,
                                                                AColumn.Name,
                                                                QuoTedStr(AColumn.Description)]));
    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TDDLSQLGeneratorPostgreSQL.GenerateCreateSequence(ASequence: TSequenceMIK): string;
begin
  Result := 'CREATE SEQUENCE "%s" INCREMENT %s START %s;';
  Result := Format(Result, [ASequence.Name,
                            IntToStr(ASequence.Increment),
                            IntToStr(ASequence.InitialValue)]);
end;

function TDDLSQLGeneratorPostgreSQL.GenerateCreateTable(ATable: TTableMIK): string;
var
  LSQL: TStringBuilder;
  LColumn: TPair<string,TColumnMIK>;
begin
  LSQL := TStringBuilder.Create;
  Result := inherited GenerateCreateTable(ATable);
  try
    if ATable.Database.Schema <> '' then
      LSQL.Append(Format(Result, [ATable.Database.Schema + '.' + ATable.Name]))
    else
      LSQL.Append(Format(Result, [ATable.Name]));
    /// <summary>
    ///   Add Colunas
    /// </summary>
    for LColumn in ATable.FieldsSort do
    begin
      LSQL.AppendLine;
      LSQL.Append('  ' + BuilderCreateFieldDefinition(LColumn.Value));
      LSQL.Append(',');
    end;
    /// <summary>
    ///   Add PrimariKey
    /// </summary>
    if ATable.PrimaryKey.Fields.Count > 0 then
    begin
      LSQL.AppendLine;
      LSQL.Append(BuilderPrimayKeyDefinition(ATable));
    end;
    /// <summary>
    ///   Add ForeignKey
    /// </summary>
//    if ATable.ForeignKeys.Count > 0 then
//    begin
//      LSQL.Append(',');
//      LSQL.Append(BuilderForeignKeyDefinition(ATable));
//    end;
    /// <summary>
    ///   Add Checks
    /// </summary>
    if ATable.Checks.Count > 0 then
    begin
      LSQL.Append(',');
      LSQL.Append(BuilderCheckDefinition(ATable));
    end;
    LSQL.AppendLine;
    LSQL.Append(');');
    /// <summary>
    ///   Add Indexe
    /// </summary>
    if ATable.IndexeKeys.Count > 0 then
      LSQL.Append(BuilderIndexeDefinition(ATable));
    LSQL.AppendLine;
    Result := LSQL.ToString;
  finally
    LSQL.Free;
  end;
end;

function TDDLSQLGeneratorPostgreSQL.GenerateDropIndexe(AIndexe: TIndexeKeyMIK): string;
begin
  Result := 'DROP INDEX %s;';
  Result := Format(Result, [AIndexe.Name]);
end;

function TDDLSQLGeneratorPostgreSQL.GenerateDropPrimaryKey(APrimaryKey: TPrimaryKeyMIK): string;
begin
  Result := 'ALTER TABLE %s DROP PRIMARY KEY;';
  Result := Format(Result, [APrimaryKey.Table.Name]);
end;

function TDDLSQLGeneratorPostgreSQL.GenerateEnableForeignKeys(AEnable: Boolean): string;
begin
  if AEnable then
    Result := ''
  else
    Result := '';
end;

function TDDLSQLGeneratorPostgreSQL.GenerateEnableTriggers(AEnable: Boolean): string;
begin
  if AEnable then
    Result := ''
  else
    Result := '';
end;

initialization
  TSQLDriverRegister.GetInstance.RegisterDriver(dnPostgreSQL, TDDLSQLGeneratorPostgreSQL.Create);

end.

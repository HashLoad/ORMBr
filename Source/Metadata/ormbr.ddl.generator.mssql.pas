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

unit ormbr.ddl.generator.mssql;

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
  TDDLSQLGeneratorMSSQL = class(TDDLSQLGenerator)
  protected
  public
    function GenerateCreateTable(ATable: TTableMIK): string; override;
    function GenerateCreateSequence(ASequence: TSequenceMIK): string; override;
    function GenerateEnableForeignKeys(AEnable: Boolean): string; override;
    function GenerateEnableTriggers(AEnable: Boolean): string; override;
  end;

implementation

{ TDDLSQLGeneratorMSSQL }

function TDDLSQLGeneratorMSSQL.GenerateCreateSequence(ASequence: TSequenceMIK): string;
begin
  Result := 'CREATE SEQUENCE %s AS int START WITH 0 INCREMENT BY %s;';
  Result := Format(Result, [ASequence.Name, IntToStr(ASequence.Increment)]);
end;

function TDDLSQLGeneratorMSSQL.GenerateCreateTable(ATable: TTableMIK): string;
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

function TDDLSQLGeneratorMSSQL.GenerateEnableForeignKeys(AEnable: Boolean): string;
begin
  if AEnable then
    Result := 'EXEC sp_MSforeachtable "ALTER TABLE ? WITH CHECK CHECK CONSTRAINT all";'
  else
    Result := 'EXEC sp_MSforeachtable "ALTER TABLE ? NOCHECK CONSTRAINT all";';
end;

function TDDLSQLGeneratorMSSQL.GenerateEnableTriggers(AEnable: Boolean): string;
begin
  if AEnable then
    Result := 'EXEC sp_MSforeachtable "ALTER TABLE ? ENABLE TRIGGER ALL";'
  else
    Result := 'EXEC sp_MSforeachtable "ALTER TABLE ? DISABLE TRIGGER ALL";';
end;

initialization
  TSQLDriverRegister.GetInstance.RegisterDriver(dnMSSQL, TDDLSQLGeneratorMSSQL.Create);

end.

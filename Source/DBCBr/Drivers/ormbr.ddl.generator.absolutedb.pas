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

unit ormbr.ddl.generator.absolutedb;

interface

uses
  SysUtils,
  StrUtils,
  Generics.Collections,
  ormbr.ddl.interfaces,
  ormbr.ddl.register,
  ormbr.ddl.generator,
  ormbr.factory.interfaces,
  ormbr.database.mapping;

type
  TDDLSQLGeneratorAbsoluteDB = class(TDDLSQLGenerator)
  protected
    function BuilderPrimayKeyDefinition(ATable: TTableMIK): string; override;
  public
    function GenerateCreateTable(ATable: TTableMIK): string; override;
    function GenerateCreateSequence(ASequence: TSequenceMIK): string; override;
    function GenerateDropTable(ATable: TTableMIK): string; override;
    function GenerateDropSequence(ASequence: TSequenceMIK): string; override;
    function GetSupportedFeatures: TSupportedFeatures; override;
  end;

implementation

{ TDDLSQLGeneratorAbsoluteDB }

function TDDLSQLGeneratorAbsoluteDB.BuilderPrimayKeyDefinition(ATable: TTableMIK): string;

  function GetPrimaryKeyColumns: string;
  var
    oColumn: TPair<string,TColumnMIK>;
  begin
    for oColumn in ATable.PrimaryKey.FieldsSort do
      Result := Result + oColumn.Value.Name + ', ';
    Result := Trim(Result);
    Delete(Result, Length(Result), 1);
  end;

begin
//  Result := 'PRIMARY KEY(%s) %s';
//  Result := Format(Result, [GetPrimaryKeyColumns, IfThen(ATable.PrimaryKey.AutoIncrement,'AUTOINCREMENT','')]);
  Result := 'PRIMARY KEY(%s) ';
  Result := Format(Result, [GetPrimaryKeyColumns]);
  Result := '  ' + Result;
end;

function TDDLSQLGeneratorAbsoluteDB.GenerateCreateSequence(ASequence: TSequenceMIK): string;
begin
  inherited;
  Result := 'INSERT INTO SQLITE_SEQUENCE (NAME, SEQ) VALUES (%s, %s);';
  Result := Format(Result, [QuotedStr(ASequence.Name), IntToStr(0)]);
end;

function TDDLSQLGeneratorAbsoluteDB.GenerateCreateTable(ATable: TTableMIK): string;
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
    /// Add Indexe
    /// </summary>
    if ATable.IndexeKeys.Count > 0 then
    begin
      oSQL.AppendLine;
      oSQL.Append(BuilderIndexeDefinition(ATable));
    end;
    oSQL.AppendLine;
    Result := oSQL.ToString;
  finally
    oSQL.Free;
  end;
end;

function TDDLSQLGeneratorAbsoluteDB.GenerateDropSequence(ASequence: TSequenceMIK): string;
begin
  inherited;
  Result := 'DELETE FROM SQLITE_SEQUENCE WHERE NAME = %s;';
  Result := Format(Result, [QuotedStr(ASequence.Name)]);
end;

function TDDLSQLGeneratorAbsoluteDB.GenerateDropTable(ATable: TTableMIK): string;
begin
  Result := inherited GenerateDropTable(ATable);
end;

function TDDLSQLGeneratorAbsoluteDB.GetSupportedFeatures: TSupportedFeatures;
begin
  Result := inherited GetSupportedFeatures - [TSupportedFeature.ForeignKeys,
                                              TSupportedFeature.Checks,
                                              TSupportedFeature.Views,
                                              TSupportedFeature.Triggers];
end;

initialization
  TSQLDriverRegister.GetInstance.RegisterDriver(dnAbsoluteDB, TDDLSQLGeneratorAbsoluteDB.Create);

end.


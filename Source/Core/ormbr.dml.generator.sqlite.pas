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
{
  @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
}

unit ormbr.dml.generator.sqlite;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  Variants,
  Rtti,
  ormbr.dml.generator,
  ormbr.driver.register,
  ormbr.dml.commands,
  ormbr.dml.cache,
  ormbr.criteria,
  dbebr.factory.interfaces,
  dbcbr.mapping.popular,
  dbcbr.mapping.classes,
  dbcbr.mapping.explorer;

type
  // Classe de conexão concreta com dbExpress
  TDMLGeneratorSQLite = class(TDMLGeneratorAbstract)
  protected
    function GetGeneratorSelect(const ACriteria: ICriteria; AOrderBy: string = ''): string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GeneratorSelectAll(AClass: TClass;
      APageSize: Integer; AID: TValue): string; override;
    function GeneratorSelectWhere(AClass: TClass; AWhere: string;
      AOrderBy: string; APageSize: Integer): string; override;
    function GeneratorAutoIncCurrentValue(AObject: TObject;
      AAutoInc: TDMLCommandAutoInc): Int64; override;
    function GeneratorAutoIncNextValue(AObject: TObject;
      AAutoInc: TDMLCommandAutoInc): Int64; override;
  end;

implementation

{ TDMLGeneratorSQLite }

constructor TDMLGeneratorSQLite.Create;
begin
  inherited;
  FDateFormat := 'yyyy-MM-dd';
  FTimeFormat := 'HH:MM:SS';
end;

destructor TDMLGeneratorSQLite.Destroy;
begin
  inherited;
end;

function TDMLGeneratorSQLite.GeneratorSelectAll(AClass: TClass;
  APageSize: Integer; AID: TValue): string;
var
  LCriteria: ICriteria;
  LTable: TTableMapping;
begin
  if not FQueryCache.TryGetValue(AClass.ClassName, Result) then
  begin
    LCriteria := GetCriteriaSelect(AClass, AID);
    Result := LCriteria.AsString;
    FQueryCache.AddOrSetValue(AClass.ClassName, Result);
  end;
  LTable := TMappingExplorer.GetMappingTable(AClass);
  // Where
  Result := Result + GetGeneratorWhere(AClass, LTable.Name, AID);
  // OrderBy
  Result := Result + GetGeneratorOrderBy(AClass, LTable.Name, AID);
  // Monta SQL para paginação
  if APageSize > -1 then
    Result := Result + GetGeneratorSelect(LCriteria);
end;

function TDMLGeneratorSQLite.GeneratorSelectWhere(AClass: TClass;
  AWhere: string; AOrderBy: string; APageSize: Integer): string;
var
  LCriteria: ICriteria;
  LScopeWhere: String;
  LScopeOrderBy: String;
begin
  if not FQueryCache.TryGetValue(AClass.ClassName, Result) then
  begin
    LCriteria := GetCriteriaSelect(AClass, -1);
    Result := LCriteria.AsString;
    FQueryCache.AddOrSetValue(AClass.ClassName, Result);
  end;
  // Scope Where
  LScopeWhere := GetGeneratorQueryScopeWhere(AClass);
  if LScopeWhere <> '' then
    Result := ' WHERE ' + LScopeWhere;
  if Length(AWhere) > 0 then
  begin
    Result := Result + IfThen(LScopeWhere = '', ' WHERE ', ' AND ');
    Result := Result + AWhere;
  end;
  // Scope Where OrderBy
  LScopeOrderBy := GetGeneratorQueryScopeOrderBy(AClass);
  if LScopeOrderBy <> '' then
    Result := ' ORDER BY ' + LScopeOrderBy;
  if Length(AOrderBy) > 0 then
  begin
    Result := Result + IfThen(LScopeOrderBy = '', ' ORDER BY ', ', ');
    Result := Result + AOrderBy;
  end;
  // Monta SQL para paginação
  if APageSize > -1 then
    Result := Result + GetGeneratorSelect(LCriteria);
end;

function TDMLGeneratorSQLite.GetGeneratorSelect(const ACriteria: ICriteria;
  AOrderBy: string): string;
begin
  Result := ' LIMIT %s OFFSET %s';
end;

function TDMLGeneratorSQLite.GeneratorAutoIncCurrentValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
var
  LSQL: String;
begin
  Result := ExecuteSequence(Format('SELECT SEQ AS SEQUENCE FROM SQLITE_SEQUENCE ' +
                                   'WHERE NAME = ''%s''', [AAutoInc.Sequence.Name]));
  if Result = 0 then
  begin
    LSQL := Format('INSERT INTO SQLITE_SEQUENCE (NAME, SEQ) VALUES (''%s'', 0)',
                   [AAutoInc.Sequence.Name]);
    FConnection.ExecuteDirect(LSQL);
  end;
end;

function TDMLGeneratorSQLite.GeneratorAutoIncNextValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
var
  LSQL: String;
begin
  Result := GeneratorAutoIncCurrentValue(AObject, AAutoInc);
  LSQL := Format('UPDATE SQLITE_SEQUENCE SET SEQ = SEQ + %s WHERE NAME = ''%s''',
                 [IntToStr(AAutoInc.Sequence.Increment), AAutoInc.Sequence.Name]);
  FConnection.ExecuteDirect(LSQL);
  Result := Result + AAutoInc.Sequence.Increment;
end;

initialization
  TDriverRegister.RegisterDriver(dnSQLite, TDMLGeneratorSQLite.Create);

end.

{
      ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)
}

unit ormbr.dml.generator.mysql;

interface

uses
  Rtti,
  Classes,
  SysUtils,
  StrUtils,
  Variants,
  ormbr.dml.generator,
  ormbr.dml.cache,
  ormbr.driver.register,
  ormbr.dml.commands,
  ormbr.criteria,
  dbcbr.mapping.classes,
  dbcbr.mapping.explorer,
  dbebr.factory.interfaces;

type
  // Classe de conex�o concreta com dbExpress
  TDMLGeneratorMySQL = class(TDMLGeneratorAbstract)
  protected
    function GetGeneratorSelect(const ACriteria: ICriteria;
      AOrderBy: string = ''): string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GeneratorSelectAll(AClass: TClass;
      APageSize: Integer; AID: Variant): string; override;
    function GeneratorSelectWhere(AClass: TClass; AWhere: string;
      AOrderBy: string; APageSize: Integer): string; override;
    function GeneratorAutoIncCurrentValue(AObject: TObject;
      AAutoInc: TDMLCommandAutoInc): Int64; override;
    function GeneratorAutoIncNextValue(AObject: TObject;
      AAutoInc: TDMLCommandAutoInc): Int64; override;
  end;

implementation

{ TDMLGeneratorMySQL }

constructor TDMLGeneratorMySQL.Create;
begin
  inherited;
  FDateFormat := 'yyyy-MM-dd';
  FTimeFormat := 'HH:MM:SS';
end;

destructor TDMLGeneratorMySQL.Destroy;
begin

  inherited;
end;

function TDMLGeneratorMySQL.GeneratorSelectAll(AClass: TClass;
  APageSize: Integer; AID: Variant): string;
var
  LCriteria: ICriteria;
  LTable: TTableMapping;
begin
  // Pesquisa se j� existe o SQL padr�o no cache, n�o tendo que montar toda vez
//  if not TQueryCache.Get.TryGetValue(AClass.ClassName, Result) then
//  begin
    LCriteria := GetCriteriaSelect(AClass, AID);
    Result := LCriteria.AsString;
    // Faz cache do comando padr�o
//    TQueryCache.Get.AddOrSetValue(AClass.ClassName, Result);
//  end;
  LTable := TMappingExplorer.GetMappingTable(AClass);
  // Where
  Result := Result + GetGeneratorWhere(AClass, LTable.Name, AID);
  // OrderBy
  Result := Result + GetGeneratorOrderBy(AClass, LTable.Name, AID);
  // Monta SQL para pagina��o
  if APageSize > -1 then
    Result := Result + GetGeneratorSelect(LCriteria);
end;

function TDMLGeneratorMySQL.GeneratorSelectWhere(AClass: TClass; AWhere: string;
  AOrderBy: string; APageSize: Integer): string;
var
  LCriteria: ICriteria;
  LScopeWhere: String;
  LScopeOrderBy: String;
begin
  // Pesquisa se j� existe o SQL padr�o no cache, n�o tendo que montar toda vez
//  if not TQueryCache.Get.TryGetValue(AClass.ClassName, Result) then
//  begin
    LCriteria := GetCriteriaSelect(AClass, -1);
    Result := LCriteria.AsString;
    // Faz cache do comando padr�o
//    TQueryCache.Get.AddOrSetValue(AClass.ClassName, Result);
//  end;
  // Scope
  LScopeWhere := GetGeneratorQueryScopeWhere(AClass);
  if LScopeWhere <> '' then
    Result := Result +' WHERE ' + LScopeWhere;
  LScopeOrderBy := GetGeneratorQueryScopeOrderBy(AClass);
  if LScopeOrderBy <> '' then
    Result := Result +' ORDER BY ' + LScopeOrderBy;
  // Params Where and OrderBy
  if Length(AWhere) > 0 then
  begin
    Result := Result + IfThen(LScopeWhere = '', ' WHERE ', ' AND ');
    Result := Result + AWhere;
  end;
  if Length(AOrderBy) > 0 then
  begin
    Result := Result + IfThen(LScopeOrderBy = '', ' ORDER BY ', ', ');
    Result := Result + AOrderBy;
  end;
  // Monta SQL para pagina��o
  if APageSize > -1 then
    Result := Result + GetGeneratorSelect(LCriteria);
end;

function TDMLGeneratorMySQL.GetGeneratorSelect(const ACriteria: ICriteria;
  AOrderBy: string): string;
begin
  Result := ' LIMIT %s OFFSET %s';
end;

function TDMLGeneratorMySQL.GeneratorAutoIncCurrentValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := ExecuteSequence('SELECT LAST_INSERT_ID()');
end;

function TDMLGeneratorMySQL.GeneratorAutoIncNextValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
//  Result := GeneratorAutoIncCurrentValue(AObject, AAutoInc);
  Result := ExecuteSequence(Format('SELECT AUTO_INCREMENT ' +
                                   'FROM INFORMATION_SCHEMA.TABLES ' +
                                   'WHERE TABLE_SCHEMA = DATABASE() ' +
                                   'AND   UPPER(TABLE_NAME) IN (%s);', [QuotedStr(AAutoInc.Sequence.TableName)]));
end;

initialization
  TDriverRegister.RegisterDriver(dnMySQL, TDMLGeneratorMySQL.Create);

end.

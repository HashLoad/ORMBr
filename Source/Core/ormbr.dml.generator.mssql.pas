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

unit ormbr.dml.generator.mssql;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  Variants,
  Rtti,
  ormbr.dml.generator,
  dbcbr.mapping.classes,
  dbcbr.mapping.explorer,
  dbebr.factory.interfaces,
  ormbr.driver.register,
  ormbr.dml.commands,
  ormbr.dml.cache,
  ormbr.criteria;

type
  // Classe de conex�o concreta com dbExpress
  TDMLGeneratorMSSql = class(TDMLGeneratorAbstract)
  protected
    function GetGeneratorSelect(const ACriteria: ICriteria;
      AOrderBy: string = ''): string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GeneratorSelectAll(AClass: TClass; APageSize: Integer;
      AID: Variant): string; override;
    function GeneratorSelectWhere(AClass: TClass; AWhere: string;
      AOrderBy: string; APageSize: Integer): string; override;
    function GeneratorAutoIncCurrentValue(AObject: TObject;
      AAutoInc: TDMLCommandAutoInc): Int64; override;
    function GeneratorAutoIncNextValue(AObject: TObject;
      AAutoInc: TDMLCommandAutoInc): Int64; override;
    function GeneratorPageNext(const ACommandSelect: string;
      APageSize, APageNext: Integer): string; override;
  end;

implementation

{ TDMLGeneratorMSSql }

constructor TDMLGeneratorMSSql.Create;
begin
  inherited;
  FDateFormat := 'dd/MM/yyyy';
  FTimeFormat := 'HH:MM:SS';
end;

destructor TDMLGeneratorMSSql.Destroy;
begin

  inherited;
end;

//function TDMLGeneratorMSSql.GetGeneratorSelect(const ACriteria: ICriteria): string;
//const
//  cSQL = 'SELECT * FROM (%s) AS %s WHERE %s';
//  cCOLUMN = 'ROW_NUMBER() OVER(ORDER BY CURRENT_TIMESTAMP) AS ROWNUMBER';
//var
//  LTable: String;
//  LWhere: String;
//begin
//  inherited;
//  LTable := ACriteria.AST.Select.TableNames.Columns[0].Name;
//  LWhere := '(ROWNUMBER <= %s) AND (ROWNUMBER > %s)';
//  ACriteria.SelectSection(secSelect);
//  ACriteria.Column(cCOLUMN);
//  Result := Format(cSQL, [ACriteria.AsString, LTable, LWhere]);
//end;

function TDMLGeneratorMSSql.GetGeneratorSelect(const ACriteria: ICriteria;
  AOrderBy: string): string;
const
  cSQL = 'SELECT * FROM (%s) AS %s WHERE %s';
  cCOLUMN = 'ROW_NUMBER() OVER(%s) AS ROWNUMBER';
var
  LTable: String;
  LWhere: String;
  LColumn: String;
begin
  inherited;
  LTable := ACriteria.AST.Select.TableNames.Columns[0].Name;
  LWhere := '(ROWNUMBER <= %s) AND (ROWNUMBER > %s)';
  ACriteria.SelectSection(secSelect);
  if AOrderBy <> '' then
  begin
    if AOrderBy.Contains('ORDER BY') then
      LColumn :=  Format(cCOLUMN, [AOrderBy])
    else
      LColumn :=  Format(cCOLUMN, ['ORDER BY ' + AOrderBy])
  end
  else
    LColumn :=  Format(cCOLUMN, ['ORDER BY CURRENT_TIMESTAMP']);

  ACriteria.Column(LColumn);
  Result := Format(cSQL, [ACriteria.AsString, LTable, LWhere]);
end;

function TDMLGeneratorMSSql.GeneratorPageNext(const ACommandSelect: string;
  APageSize, APageNext: Integer): string;
begin
  if APageSize > -1 then
    Result := Format(ACommandSelect, [IntToStr(APageNext + APageSize), IntToStr(APageNext)])
  else
    Result := ACommandSelect;
end;

function TDMLGeneratorMSSql.GeneratorSelectAll(AClass: TClass;
  APageSize: Integer; AID: Variant): string;
var
  LCriteria: ICriteria;
  LTable: TTableMapping;
  LOrderBy: string;
begin
  LTable := TMappingExplorer.GetMappingTable(AClass);
  LOrderBy := GetGeneratorOrderBy(AClass, LTable.Name, AID);

  // Pesquisa se j� existe o SQL padr�o no cache, n�o tendo que montar toda vez
//  if not TQueryCache.Get.TryGetValue(AClass.ClassName, Result) then
//  begin
    LCriteria := GetCriteriaSelect(AClass, AID);
    Result := LCriteria.AsString;
    // Atualiza o comando SQL com pagina��o e atualiza a lista de cache.
    if APageSize > -1 then
      Result := GetGeneratorSelect(LCriteria, LOrderBy);
    // Faz cache do comando padr�o
//    TQueryCache.Get.AddOrSetValue(AClass.ClassName, Result);
//  end;

  // Where
  Result := Result + GetGeneratorWhere(AClass, LTable.Name, AID);
  // OrderBy
  Result := Result + LOrderBy;
end;

function TDMLGeneratorMSSql.GeneratorSelectWhere(AClass: TClass; AWhere: string;
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
    // Atualiza o comando SQL com pagina��o e atualiza a lista de cache.
    if APageSize > -1 then
      Result := GetGeneratorSelect(LCriteria, AOrderBy);
    // Faz cache do comando padr�o
//    TQueryCache.Get.AddOrSetValue(AClass.ClassName, Result);
//  end;
  // Scope
  LScopeWhere := GetGeneratorQueryScopeWhere(AClass);
  if LScopeWhere <> '' then
    Result := ' WHERE ' + LScopeWhere;
  LScopeOrderBy := GetGeneratorQueryScopeOrderBy(AClass);
  if LScopeOrderBy <> '' then
    Result := ' ORDER BY ' + LScopeOrderBy;
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
end;

function TDMLGeneratorMSSql.GeneratorAutoIncCurrentValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := ExecuteSequence(Format('SELECT CURRENT_VALUE FROM SYS.SEQUENCES WHERE NAME = ''%s''',
                                   [AAutoInc.Sequence.Name]) );
end;

function TDMLGeneratorMSSql.GeneratorAutoIncNextValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := ExecuteSequence(Format('SELECT NEXT VALUE FOR %s ',
                                   [AAutoInc.Sequence.Name]));
end;

initialization
  TDriverRegister.RegisterDriver(dnMSSQL, TDMLGeneratorMSSql.Create);
end.

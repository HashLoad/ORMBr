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
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @author(Skype : ispinheiro)
}

unit ormbr.dml.generator.oracle;

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
  TDMLGeneratorOracle = class(TDMLGeneratorAbstract)
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
    function GeneratorPageNext(const ACommandSelect: string;
      APageSize: Integer; APageNext: Integer): string; override;
  end;

implementation

const
  cSELECTROW = 'SELECT * FROM ( ' + sLineBreak +
               '   SELECT T.*, ROWNUM AS ROWINI FROM (%s) T';

{ TDMLGeneratorOracle }

constructor TDMLGeneratorOracle.Create;
begin
  inherited;
  FDateFormat := 'yyyy-MM-dd';
  FTimeFormat := 'HH:MM:SS';
end;

destructor TDMLGeneratorOracle.Destroy;
begin
  inherited;
end;

function TDMLGeneratorOracle.GeneratorPageNext(const ACommandSelect: string;
  APageSize: Integer; APageNext: Integer): string;
begin
  if APageSize > -1 then
    Result := Format(ACommandSelect, [IntToStr(APageNext + APageSize), IntToStr(APageNext)])
  else
    Result := ACommandSelect;
end;

function TDMLGeneratorOracle.GeneratorSelectAll(AClass: TClass;
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
    // Atualiza o comando SQL com pagina��o e atualiza a lista de cache.
    if APageSize > -1 then
      Result := Format(cSELECTROW, [Result]);
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
    Result := Result + sLineBreak + GetGeneratorSelect(LCriteria);
end;

function TDMLGeneratorOracle.GeneratorSelectWhere(AClass: TClass; AWhere: string;
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
    if APageSize > -1 then
      Result := Format(cSELECTROW, [Result]);
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
  if APageSize > -1 then
     Result := Result + sLineBreak + GetGeneratorSelect(LCriteria);
end;

function TDMLGeneratorOracle.GetGeneratorSelect(const ACriteria: ICriteria;
  AOrderBy: string): string;
begin
  Result := '   WHERE ROWNUM <= %s) ' + sLineBreak +
            'WHERE ROWINI > %s';
end;

function TDMLGeneratorOracle.GeneratorAutoIncCurrentValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := ExecuteSequence(Format('SELECT %s.CURRVAL FROM DUAL',
                                   [AAutoInc.Sequence.Name]));
end;

function TDMLGeneratorOracle.GeneratorAutoIncNextValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := ExecuteSequence(Format('SELECT %s.NEXTVAL FROM DUAL',
                                   [AAutoInc.Sequence.Name]));
end;

initialization
  TDriverRegister.RegisterDriver(dnOracle, TDMLGeneratorOracle.Create);

end.


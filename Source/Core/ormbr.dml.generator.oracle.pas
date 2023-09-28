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
  // Classe de conexão concreta com dbExpress
  TDMLGeneratorOracle = class(TDMLGeneratorAbstract)
  protected
    function GetGeneratorSelect(const ACriteria: ICriteria;
      AOrderBy: string = ''): string; override;
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
    function GeneratorPageNext(const ACommandSelect: string;
      APageSize: Integer; APageNext: Integer): string; override;
  end;

implementation

const
  SELECTROW = 'SELECT * FROM ( ' + sLineBreak +
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
  if APageSize > -1 then
    Result := Format(SELECTROW, [Result]);
  LTable := TMappingExplorer.GetMappingTable(AClass);
  // Where
  Result := Result + GetGeneratorWhere(AClass, LTable.Name, AID);
  // OrderBy
  Result := Result + GetGeneratorOrderBy(AClass, LTable.Name, AID);
  // Monta SQL para paginação
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
  if not FQueryCache.TryGetValue(AClass.ClassName, Result) then
  begin
    LCriteria := GetCriteriaSelect(AClass, -1);
    Result := LCriteria.AsString;
    if APageSize > -1 then
      Result := Format(SELECTROW, [Result]);
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
  // Scope OrderBy
  LScopeOrderBy := GetGeneratorQueryScopeOrderBy(AClass);
  if LScopeOrderBy <> '' then
    Result := ' ORDER BY ' + LScopeOrderBy;
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


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
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)
}

unit ormbr.dml.generator.ads;

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
  // Classe de banco de dados ADS
  TDMLGeneratorADS = class(TDMLGeneratorAbstract)
  protected
    function GetGeneratorSelect(const ACriteria: ICriteria;
      const APageIndex: integer; const AOrderBy: string = ''): string; reintroduce;
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

{ TDMLGeneratorADS }

constructor TDMLGeneratorADS.Create;
begin
  inherited;
  FDateFormat := 'DD/MM/CCYY';
  FTimeFormat := 'HH:MM:SS';
end;

destructor TDMLGeneratorADS.Destroy;
begin

  inherited;
end;

function TDMLGeneratorADS.GetGeneratorSelect(const ACriteria: ICriteria;
  const APageIndex: integer; const AOrderBy: string): string;
var
  LFirstColumn: string;
begin
  LFirstColumn := ACriteria.AST.Select.Columns.Columns[0].Name;
  if APageIndex > -1 then
    LFirstColumn := Format(LFirstColumn, ['FIRST %s SKIP %s '])
  else
    LFirstColumn := Format(LFirstColumn, ['']);
  ACriteria.AST.Select.Columns.Columns[0].Name := LFirstColumn;
  Result := ACriteria.AsString;
end;

function TDMLGeneratorADS.GeneratorSelectAll(AClass: TClass;
  APageSize: Integer; AID: TValue): string;
var
  LCriteria: ICriteria;
  LTable: TTableMapping;
begin
  if not FQueryCache.TryGetValue(AClass.ClassName, Result) then
  begin
    LCriteria := GetCriteriaSelect(AClass, AID);
    LCriteria.AST.Select
             .Columns
             .Columns[0].Name := '%s' + LCriteria.AST.Select
                                                     .Columns
                                                     .Columns[0].Name;
    Result := GetGeneratorSelect(LCriteria, APageSize);
    FQueryCache.AddOrSetValue(AClass.ClassName, Result);
  end;
  LTable := TMappingExplorer.GetMappingTable(AClass);
  // Where
  Result := Result + GetGeneratorWhere(AClass, LTable.Name, AID);
  // OrderBy
  Result := Result + GetGeneratorOrderBy(AClass, LTable.Name, AID);
end;

function TDMLGeneratorADS.GeneratorSelectWhere(AClass: TClass;
  AWhere: string; AOrderBy: string; APageSize: Integer): string;
var
  LCriteria: ICriteria;
  LScopeWhere: String;
  LScopeOrderBy: String;
begin
  if not FQueryCache.TryGetValue(AClass.ClassName, Result) then
  begin
    LCriteria := GetCriteriaSelect(AClass, -1);
    LCriteria.AST.Select
             .Columns
             .Columns[0].Name := '%s' + LCriteria.AST.Select
                                                     .Columns
                                                     .Columns[0].Name;
    Result := GetGeneratorSelect(LCriteria, APageSize);
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
end;

function TDMLGeneratorADS.GeneratorAutoIncCurrentValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := ExecuteSequence(Format('SELECT GEN_ID(%s, 0) FROM RDB$DATABASE;',
                                   [AAutoInc.Sequence.Name]));
end;

function TDMLGeneratorADS.GeneratorAutoIncNextValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := ExecuteSequence(Format('SELECT GEN_ID(%s, %s) FROM RDB$DATABASE;',
                                   [AAutoInc.Sequence.Name,
                           IntToStr(AAutoInc.Sequence.Increment)]));
end;

initialization
  TDriverRegister.RegisterDriver(dnADS, TDMLGeneratorADS.Create);

end.

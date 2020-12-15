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

  ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.dml.generator.firebird;

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
  dbcbr.mapping.classes,
  dbcbr.mapping.explorer,
  dbebr.factory.interfaces;

type
  // Classe de banco de dados Firebird
  TDMLGeneratorFirebird = class(TDMLGeneratorAbstract)
  protected
    function GetGeneratorSelect(const ACriteria: ICriteria): string; override;
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

{ TDMLGeneratorFirebird }

constructor TDMLGeneratorFirebird.Create;
begin
  inherited;
  FDateFormat := 'MM/dd/yyyy';
  FTimeFormat := 'HH:MM:SS';
end;

destructor TDMLGeneratorFirebird.Destroy;
begin

  inherited;
end;

function TDMLGeneratorFirebird.GetGeneratorSelect(const ACriteria: ICriteria): string;
begin
  inherited;
  ACriteria.AST.Select
           .Columns
           .Columns[0].Name := 'FIRST %s SKIP %s ' + ACriteria.AST.Select
                                                              .Columns
                                                              .Columns[0].Name;
  Result := ACriteria.AsString;
end;

function TDMLGeneratorFirebird.GeneratorSelectAll(AClass: TClass;
  APageSize: Integer; AID: Variant): string;
var
  LCriteria: ICriteria;
  LTable: TTableMapping;
begin
  // Pesquisa se j� existe o SQL padr�o no cache, n�o tendo que montar toda vez
  if not TDMLCache.DMLCache.TryGetValue(AClass.ClassName, Result) then
  begin
    LCriteria := GetCriteriaSelect(AClass, AID);
    Result := LCriteria.AsString;
    // Atualiza o comando SQL com pagina��o e atualiza a lista de cache.
    if APageSize > -1 then
      Result := GetGeneratorSelect(LCriteria);
    // Faz cache do comando padr�o
    TDMLCache.DMLCache.AddOrSetValue(AClass.ClassName, Result);
  end;
  LTable := TMappingExplorer.GetMappingTable(AClass);
  // Where
  Result := Result + GetGeneratorWhere(AClass, LTable.Name, AID);
  // OrderBy
  Result := Result + GetGeneratorOrderBy(AClass, LTable.Name, AID);
end;

function TDMLGeneratorFirebird.GeneratorSelectWhere(AClass: TClass;
  AWhere: string; AOrderBy: string; APageSize: Integer): string;
var
  LCriteria: ICriteria;
begin
  // Pesquisa se j� existe o SQL padr�o no cache, n�o tendo que montar toda vez
  if not TDMLCache.DMLCache.TryGetValue(AClass.ClassName, Result) then
  begin
    LCriteria := GetCriteriaSelect(AClass, -1);
    Result := LCriteria.AsString;
    // Atualiza o comando SQL com pagina��o e atualiza a lista de cache.
    if APageSize > -1 then
      Result := GetGeneratorSelect(LCriteria);
    // Faz cache do comando padr�o
    TDMLCache.DMLCache.AddOrSetValue(AClass.ClassName, Result);
  end;
  if Length(AWhere) > 0 then
    Result := Result + ' WHERE ' + AWhere;
  if Length(AOrderBy) > 0 then
    Result := Result + ' ORDER BY ' + AOrderBy;
end;

function TDMLGeneratorFirebird.GeneratorAutoIncCurrentValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := ExecuteSequence(Format('SELECT GEN_ID(%s, 0) FROM RDB$DATABASE;',
                                   [AAutoInc.Sequence.Name]));
end;

function TDMLGeneratorFirebird.GeneratorAutoIncNextValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := ExecuteSequence(Format('SELECT GEN_ID(%s, %s) FROM RDB$DATABASE;',
                                   [AAutoInc.Sequence.Name,
                           IntToStr(AAutoInc.Sequence.Increment)]));
end;

initialization
  TDriverRegister.RegisterDriver(dnFirebird, TDMLGeneratorFirebird.Create);

end.

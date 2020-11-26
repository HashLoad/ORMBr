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

unit ormbr.dml.generator.nexusdb;

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
  ormbr.criteria;

type
  // Classe de banco de dados AbsoluteDB
  TDMLGeneratorNexusDB = class(TDMLGeneratorAbstract)
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
    function GeneratorPageNext(const ACommandSelect: string;
      APageSize, APageNext: Integer): string; override;
  end;

implementation

{ TDMLGeneratorNexusDB }

constructor TDMLGeneratorNexusDB.Create;
begin
  inherited;
  FDateFormat := 'yyyy-mm-dd';
  FTimeFormat := 'HH:MM:SS';
end;

destructor TDMLGeneratorNexusDB.Destroy;
begin

  inherited;
end;

function TDMLGeneratorNexusDB.GetGeneratorSelect(
  const ACriteria: ICriteria): string;
begin
  inherited;
  ACriteria.AST.Select.Columns.Columns[0].Name := 'TOP %s, %s '
                                                + ACriteria.AST.Select.Columns.Columns[0].Name;
  Result := ACriteria.AsString;
end;

function TDMLGeneratorNexusDB.GeneratorPageNext(const ACommandSelect: string;
  APageSize, APageNext: Integer): string;
begin
  if APageNext = 0 then
    APageNext := 1;

  if APageSize > 0 then
    Result := Format(ACommandSelect, [IntToStr(APageSize), IntToStr(APageNext)])
  else
    Result := ACommandSelect;
end;

function TDMLGeneratorNexusDB.GeneratorSelectAll(AClass: TClass;
  APageSize: Integer; AID: Variant): string;
var
  LCriteria: ICriteria;
  LTable: TTableMapping;
  LPrimaryKey: TPrimaryKeyMapping;
  LOrderBy: TOrderByMapping;
  LOrderByList: TStringList;
  LColumnName: String;
  LFor: Integer;
begin
  // Pesquisa se j� existe o SQL padr�o no cache, n�o tendo que montar toda vez
  if not FDMLCriteria.TryGetValue(AClass.ClassName, Result) then
  begin
    LCriteria := GetCriteriaSelect(AClass, AID);
    Result := LCriteria.AsString;
    // Atualiza o comando SQL com pagina��o e atualiza a lista de cache.
    if APageSize > -1 then
      Result := GetGeneratorSelect(LCriteria);
    // Faz cache do comando padr�o
    FDMLCriteria.AddOrSetValue(AClass.ClassName, Result);
  end;
  LTable := TMappingExplorer.GetInstance.GetMappingTable(AClass);
  // Where
  if VarToStr(AID) <> '-1' then
  begin
    LPrimaryKey := TMappingExplorer.GetInstance.GetMappingPrimaryKey(AClass);
    if LPrimaryKey <> nil then
    begin
      Result := Result + ' WHERE %s ';
      for LFor := 0 to LPrimaryKey.Columns.Count -1 do
      begin
        if LFor > 0 then
         Continue;
        LColumnName := LTable.Name + '.' + LPrimaryKey.Columns[LFor];
        if TVarData(AID).VType = varInteger then
          Result := Result + LColumnName + ' = ' + IntToStr(AID)
        else
          Result := Result + LColumnName + ' = ' + QuotedStr(AID);
      end;
    end;
  end;
  // OrderBy
  LOrderBy := TMappingExplorer.GetInstance.GetMappingOrderBy(AClass);
  if LOrderBy <> nil then
  begin
    Result := Result + ' ORDER BY ';
    LOrderByList := TStringList.Create;
    try
      LOrderByList.Duplicates := dupError;
      ExtractStrings([',', ';'], [' '], PChar(LOrderBy.ColumnsName), LOrderByList);
      for LFor := 0 to LOrderByList.Count -1 do
      begin
        Result := Result + LTable.Name + '.' + LOrderByList[LFor];
        if LFor < LOrderByList.Count -1 then
          Result := Result + ', ';
      end;
    finally
      LOrderByList.Free;
    end;
  end;
end;

function TDMLGeneratorNexusDB.GeneratorSelectWhere(AClass: TClass; AWhere,
  AOrderBy: string; APageSize: Integer): string;
var
  LCriteria: ICriteria;
begin
  // Pesquisa se j� existe o SQL padr�o no cache, n�o tendo que montar toda vez
  if not FDMLCriteria.TryGetValue(AClass.ClassName, Result) then
  begin
    LCriteria := GetCriteriaSelect(AClass, -1);
    Result := LCriteria.AsString;
    // Atualiza o comando SQL com pagina��o e atualiza a lista de cache.
    if APageSize > -1 then
      Result := GetGeneratorSelect(LCriteria);
    // Faz cache do comando padr�o
    FDMLCriteria.AddOrSetValue(AClass.ClassName, Result);
  end;
  Result := Result + ' WHERE ' + AWhere;
  if Length(AOrderBy) > 0 then
    Result := Result + ' ORDER BY ' + AOrderBy;
end;

function TDMLGeneratorNexusDB.GeneratorAutoIncCurrentValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := ExecuteSequence(Format('SELECT MAX(%s) FROM %s',
                                   [AAutoInc.PrimaryKey.Columns.Items[0],
                                    AAutoInc.Sequence.TableName]));
end;

function TDMLGeneratorNexusDB.GeneratorAutoIncNextValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := GeneratorAutoIncCurrentValue(AObject, AAutoInc)
          + AAutoInc.Sequence.Increment;
end;

initialization
  TDriverRegister.RegisterDriver(dnNexusDB, TDMLGeneratorNexusDB.Create);

end.

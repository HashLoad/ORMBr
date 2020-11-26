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

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.dml.generator.sqlite;

interface

uses
  Classes,
  SysUtils,
  Variants,
  Rtti,
  ormbr.dml.generator,
  ormbr.driver.register,
  ormbr.dml.commands,
  ormbr.criteria,
  dbebr.factory.interfaces,
  dbcbr.mapping.classes,
  dbcbr.mapping.explorer;

type
  // Classe de conexão concreta com dbExpress
  TDMLGeneratorSQLite = class(TDMLGeneratorAbstract)
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
  // Pesquisa se já existe o SQL padrão no cache, não tendo que montar toda vez
  if not FDMLCriteria.TryGetValue(AClass.ClassName, Result) then
  begin
    LCriteria := GetCriteriaSelect(AClass, AID);
    Result := LCriteria.AsString;
    // Faz cache do comando padrão
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
  // Monta SQL para paginação
  if APageSize > -1 then
    Result := Result + GetGeneratorSelect(LCriteria);
end;

function TDMLGeneratorSQLite.GeneratorSelectWhere(AClass: TClass;
  AWhere: string; AOrderBy: string; APageSize: Integer): string;
var
  LCriteria: ICriteria;
begin
  // Pesquisa se já existe o SQL padrão no cache, não tendo que montar toda vez
  if not FDMLCriteria.TryGetValue(AClass.ClassName, Result) then
  begin
    LCriteria := GetCriteriaSelect(AClass, -1);
    Result := LCriteria.AsString;
    // Faz cache do comando padrão
    FDMLCriteria.AddOrSetValue(AClass.ClassName, Result);
  end;
  Result := Result + ' WHERE ' + AWhere;
  Result := Result + 'ORDER BY ' + AOrderBy;
  // Monta SQL para paginação
  if APageSize > -1 then
    Result := Result + GetGeneratorSelect(LCriteria);
end;

function TDMLGeneratorSQLite.GetGeneratorSelect(const ACriteria: ICriteria): string;
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

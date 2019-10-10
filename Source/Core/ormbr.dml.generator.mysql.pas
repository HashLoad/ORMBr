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

unit ormbr.dml.generator.mysql;

interface

uses
  Rtti,
  Classes,
  SysUtils,
  Variants,
  ormbr.dml.generator,
  ormbr.mapping.classes,
  ormbr.mapping.explorer,
  ormbr.factory.interfaces,
  ormbr.driver.register,
  ormbr.dml.commands,
  ormbr.criteria;

type
  /// <summary>
  /// Classe de conexão concreta com dbExpress
  /// </summary>
  TDMLGeneratorMySQL = class(TDMLGeneratorAbstract)
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
  LTable: TTableMapping;
  LCriteria: ICriteria;
  LOrderBy: TOrderByMapping;
  LOrderByList: TStringList;
  LFor: Integer;
begin
  LTable := TMappingExplorer.GetInstance.GetMappingTable(AClass);
  LCriteria := GetCriteriaSelect(AClass, AID);
  /// OrderBy
  LOrderBy := TMappingExplorer.GetInstance.GetMappingOrderBy(AClass);
  if LOrderBy <> nil then
  begin
    LOrderByList := TStringList.Create;
    try
      LOrderByList.Duplicates := dupError;
      ExtractStrings([',', ';'], [' '], PChar(LOrderBy.ColumnsName), LOrderByList);
      for LFor := 0 to LOrderByList.Count -1 do
        LCriteria.OrderBy(LTable.Name + '.' + LOrderByList[LFor]);
    finally
      LOrderByList.Free;
    end;
  end;
  Result := LCriteria.AsString;
  if APageSize > -1 then
    Result := GetGeneratorSelect(LCriteria);
end;

function TDMLGeneratorMySQL.GeneratorSelectWhere(AClass: TClass; AWhere: string;
  AOrderBy: string; APageSize: Integer): string;
var
  LCriteria: ICriteria;
begin
  LCriteria := GetCriteriaSelect(AClass, -1);
  LCriteria.Where(AWhere);
  LCriteria.OrderBy(AOrderBy);
  Result := LCriteria.AsString;
  if APageSize > -1 then
    Result := GetGeneratorSelect(LCriteria);
end;

function TDMLGeneratorMySQL.GetGeneratorSelect(const ACriteria: ICriteria): string;
begin
  Result := ACriteria.AsString;
  if FDMLCriteriaFound then
    Exit;
  Result := ACriteria.AsString + ' LIMIT %s OFFSET %s';
end;

function TDMLGeneratorMySQL.GeneratorAutoIncCurrentValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := ExecuteSequence('SELECT LAST_INSERT_ID()');
end;

function TDMLGeneratorMySQL.GeneratorAutoIncNextValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := GeneratorAutoIncCurrentValue(AObject, AAutoInc);
//  Result := ExecuteSequence(Format('SELECT AUTO_INCREMENT ' +
//                                   'FROM INFORMATION_SCHEMA.TABLES ' +
//                                   'WHERE TABLE_SCHEMA = DATABASE() ' +
//                                   'AND   UPPER(TABLE_NAME) IN (%s);', [QuotedStr(AAutoInc.Sequence.TableName)]));
end;

initialization
  TDriverRegister.RegisterDriver(dnMySQL, TDMLGeneratorMySQL.Create);

end.


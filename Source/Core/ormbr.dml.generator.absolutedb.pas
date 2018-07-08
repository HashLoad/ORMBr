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

unit ormbr.dml.generator.absolutedb;

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  Rtti,
  ormbr.dml.generator,
  ormbr.mapping.classes,
  ormbr.mapping.explorer,
  ormbr.factory.interfaces,
  ormbr.driver.register,
  ormbr.types.database,
  ormbr.dml.commands,
  ormbr.criteria;

type
  /// <summary>
  /// Classe de banco de dados AbsoluteDB
  /// </summary>
  TDMLGeneratorAbsoluteDB = class(TDMLGeneratorAbstract)
  protected
    function GetGeneratorSelect(ACriteria: ICriteria): string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GeneratorSelectAll(AClass: TClass; APageSize: Integer; AID: Variant): string; override;
    function GeneratorSelectWhere(AClass: TClass; AWhere: string; AOrderBy: string; APageSize: Integer): string; override;
    function GeneratorSequenceCurrentValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64; override;
    function GeneratorSequenceNextValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64; override;
  end;

implementation

{ TDMLGeneratorAbsoluteDB }

constructor TDMLGeneratorAbsoluteDB.Create;
begin
  inherited;
  FDateFormat := 'dd/MM/yyyy';
  FTimeFormat := 'HH:MM:SS';
end;

destructor TDMLGeneratorAbsoluteDB.Destroy;
begin

  inherited;
end;

function TDMLGeneratorAbsoluteDB.GetGeneratorSelect(ACriteria: ICriteria): string;
begin
  inherited;
  ACriteria.AST.Select.Columns.Columns[0].Name := 'TOP %s, %s ' + ACriteria.AST.Select.Columns.Columns[0].Name;
  Result := ACriteria.AsString;
end;

function TDMLGeneratorAbsoluteDB.GeneratorSelectAll(AClass: TClass;
  APageSize: Integer; AID: Variant): string;
var
  LTable: TTableMapping;
  LCriteria: ICriteria;
  LOrderBy: TOrderByMapping;
  LOrderByList: TStringList;
  LFor: Integer;
begin
  LTable := TMappingExplorer
              .GetInstance
                .GetMappingTable(AClass);
  LCriteria := GetCriteriaSelect(AClass, AID);
  /// OrderBy
  LOrderBy := TMappingExplorer
                .GetInstance
                  .GetMappingOrderBy(AClass);
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
  if APageSize > -1 then
     Result := GetGeneratorSelect(LCriteria)
  else
     Result := LCriteria.AsString;
end;

function TDMLGeneratorAbsoluteDB.GeneratorSelectWhere(AClass: TClass; AWhere,
  AOrderBy: string; APageSize: Integer): string;
var
  LCriteria: ICriteria;
begin
  LCriteria := GetCriteriaSelect(AClass, -1);
  LCriteria.Where(AWhere);
  LCriteria.OrderBy(AOrderBy);
  if APageSize > -1 then
     Result := LCriteria.AsString + ' LIMIT %s OFFSET %s'
  else
     Result := LCriteria.AsString;
end;

function TDMLGeneratorAbsoluteDB.GeneratorSequenceCurrentValue(AObject: TObject;
  ACommandInsert: TDMLCommandInsert): Int64;
begin
  Result := ExecuteSequence(Format('SELECT LASTAUTOINC(%s, %s) FROM %s', [ACommandInsert.Table.Name,
                                                                          ACommandInsert.Table.Name,
                                                                          ACommandInsert.Table.Name]));
end;

function TDMLGeneratorAbsoluteDB.GeneratorSequenceNextValue(AObject: TObject;
  ACommandInsert: TDMLCommandInsert): Int64;
begin
  Result := GeneratorSequenceCurrentValue(AObject, ACommandInsert) + ACommandInsert.Sequence.Increment;
end;

initialization
  TDriverRegister.RegisterDriver(dnAbsoluteDB, TDMLGeneratorAbsoluteDB.Create);

end.

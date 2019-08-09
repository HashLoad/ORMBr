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

unit ormbr.dml.generator.elevatedb;

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
  ormbr.dml.commands,
  ormbr.criteria;

type
  /// <summary>
  /// Classe de banco de dados AbsoluteDB
  /// </summary>
  TDMLGeneratorElevateDB = class(TDMLGeneratorAbstract)
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

{ TDMLGeneratorElevateDB }

constructor TDMLGeneratorElevateDB.Create;
begin
  inherited;
  FDateFormat := 'dd/MM/yyyy';
  FTimeFormat := 'HH:MM:SS';
end;

destructor TDMLGeneratorElevateDB.Destroy;
begin

  inherited;
end;

function TDMLGeneratorElevateDB.GetGeneratorSelect(
  const ACriteria: ICriteria): string;
begin
  inherited;
  Result := ACriteria.AsString;
  if FDMLCriteriaFound then
    Exit;
  ACriteria.AST.Select.Columns.Columns[0].Name := 'TOP %s, %s '
                                                + ACriteria.AST.Select.Columns.Columns[0].Name;
  Result := ACriteria.AsString;
end;

function TDMLGeneratorElevateDB.GeneratorPageNext(const ACommandSelect: string;
  APageSize, APageNext: Integer): string;
begin
  if APageNext = 0 then
    APageNext := 1;

  if APageSize > 0 then
    Result := Format(ACommandSelect, [IntToStr(APageSize), IntToStr(APageNext)])
  else
    Result := ACommandSelect;
end;

function TDMLGeneratorElevateDB.GeneratorSelectAll(AClass: TClass;
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

function TDMLGeneratorElevateDB.GeneratorSelectWhere(AClass: TClass; AWhere,
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

function TDMLGeneratorElevateDB.GeneratorAutoIncCurrentValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := ExecuteSequence(Format('SELECT MAX(%s) FROM %s',
                                   [AAutoInc.PrimaryKey.Columns.Items[0],
                                    AAutoInc.Sequence.TableName]));
end;

function TDMLGeneratorElevateDB.GeneratorAutoIncNextValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := GeneratorAutoIncCurrentValue(AObject, AAutoInc)
          + AAutoInc.Sequence.Increment;
end;

initialization
  TDriverRegister.RegisterDriver(dnAbsoluteDB, TDMLGeneratorElevateDB.Create);

end.

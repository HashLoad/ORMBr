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
  ormbr.dml.commands,
  ormbr.criteria;

type
  /// <summary>
  ///   Classe de banco de dados AbsoluteDB
  /// </summary>
  TDMLGeneratorAbsoluteDB = class(TDMLGeneratorAbstract)
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

function TDMLGeneratorAbsoluteDB.GetGeneratorSelect(
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

function TDMLGeneratorAbsoluteDB.GeneratorSelectAll(AClass: TClass;
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

function TDMLGeneratorAbsoluteDB.GeneratorSelectWhere(AClass: TClass; AWhere,
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

function TDMLGeneratorAbsoluteDB.GeneratorAutoIncCurrentValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := ExecuteSequence(Format('SELECT LASTAUTOINC(%s, %s) FROM %s',
                                   [AAutoInc.Sequence.TableName,
                                    AAutoInc.PrimaryKey.Columns.Items[0],
                                    AAutoInc.Sequence.TableName]));
end;

function TDMLGeneratorAbsoluteDB.GeneratorAutoIncNextValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := GeneratorAutoIncCurrentValue(AObject, AAutoInc)
          + AAutoInc.Sequence.Increment;
end;

initialization
  TDriverRegister.RegisterDriver(dnAbsoluteDB, TDMLGeneratorAbsoluteDB.Create);

end.

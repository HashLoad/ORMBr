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

unit ormbr.dml.generator.mssql;

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
  /// Classe de conexão concreta com dbExpress
  /// </summary>
  TDMLGeneratorMSSql = class(TDMLGeneratorAbstract)
  protected
    function GetGeneratorSelect(const ACriteria: ICriteria): string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GeneratorSelectAll(AClass: TClass; APageSize: Integer;
      AID: Variant): string; override;
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

{ TDMLGeneratorMSSql }

constructor TDMLGeneratorMSSql.Create;
begin
  inherited;
  FDateFormat := 'dd/MM/yyyy';
  FTimeFormat := 'HH:MM:SS';
end;

destructor TDMLGeneratorMSSql.Destroy;
begin

  inherited;
end;

function TDMLGeneratorMSSql.GetGeneratorSelect(const ACriteria: ICriteria): string;
const
  cSQL = 'SELECT * FROM (%s) AS %s WHERE %s';
  cCOLUMN = 'ROW_NUMBER() OVER(ORDER BY CURRENT_TIMESTAMP) AS ROWNUMBER';
var
  LTable: String;
  LWhere: String;
begin
  inherited;
  Result := ACriteria.AsString;
  if FDMLCriteriaFound then
    Exit;
  LTable := ACriteria.AST.Select.TableNames.Columns[0].Name;
  LWhere := '(ROWNUMBER <= %s) AND (ROWNUMBER > %s)';
  ACriteria.SelectSection(secSelect);
  ACriteria.Column(cCOLUMN);
  Result := Format(cSQL, [ACriteria.AsString, LTable, LWhere]);
end;

function TDMLGeneratorMSSql.GeneratorPageNext(const ACommandSelect: string;
  APageSize, APageNext: Integer): string;
begin
  if APageSize > -1 then
    Result := Format(ACommandSelect, [IntToStr(APageNext + APageSize), IntToStr(APageNext)])
  else
    Result := ACommandSelect;
end;

function TDMLGeneratorMSSql.GeneratorSelectAll(AClass: TClass;
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
  Result := LCriteria.AsString;
  if APageSize > -1 then
    Result := GetGeneratorSelect(LCriteria);
  /// OrderBy
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

function TDMLGeneratorMSSql.GeneratorSelectWhere(AClass: TClass; AWhere: string;
  AOrderBy: string; APageSize: Integer): string;
var
  LCriteria: ICriteria;
begin
  LCriteria := GetCriteriaSelect(AClass, -1);
  LCriteria.Where(AWhere);
  Result := LCriteria.AsString;
  if APageSize > -1 then
     Result := GetGeneratorSelect(LCriteria);

  if Length(AOrderBy) > 0 then
    Result := Result + ' ORDER BY ' + AOrderBy;
end;

function TDMLGeneratorMSSql.GeneratorAutoIncCurrentValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := ExecuteSequence(Format('SELECT CURRENT_VALUE FROM SYS.SEQUENCES WHERE NAME = ''%s''',
                                   [AAutoInc.Sequence.Name]) );
end;

function TDMLGeneratorMSSql.GeneratorAutoIncNextValue(AObject: TObject;
  AAutoInc: TDMLCommandAutoInc): Int64;
begin
  Result := ExecuteSequence(Format('SELECT NEXT VALUE FOR %s ',
                                   [AAutoInc.Sequence.Name]));
end;

initialization
  TDriverRegister.RegisterDriver(dnMSSQL, TDMLGeneratorMSSql.Create);

end.

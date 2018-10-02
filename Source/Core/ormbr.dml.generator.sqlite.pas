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
  Rtti,
  ormbr.dml.generator,
  ormbr.driver.register,
  ormbr.factory.interfaces,
  ormbr.mapping.classes,
  ormbr.mapping.explorer,
  ormbr.dml.commands,
  ormbr.criteria;

type
  /// <summary>
  /// Classe de conexão concreta com dbExpress
  /// </summary>
  TDMLGeneratorSQLite = class(TDMLGeneratorAbstract)
  public
    constructor Create; override;
    destructor Destroy; override;
    function GeneratorSelectAll(AClass: TClass; APageSize: Integer; AID: Variant): string; override;
    function GeneratorSelectWhere(AClass: TClass; AWhere: string; AOrderBy: string; APageSize: Integer): string; override;
    function GeneratorSequenceCurrentValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64; override;
    function GeneratorSequenceNextValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64; override;
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
     Result := LCriteria.AsString + ' LIMIT %s OFFSET %s'
  else
     Result := LCriteria.AsString;
end;

function TDMLGeneratorSQLite.GeneratorSelectWhere(AClass: TClass;
  AWhere: string; AOrderBy: string; APageSize: Integer): string;
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

function TDMLGeneratorSQLite.GeneratorSequenceCurrentValue(AObject: TObject;
  ACommandInsert: TDMLCommandInsert): Int64;
var
  LSQL: String;
begin
  Result := ExecuteSequence(Format('SELECT SEQ AS SEQUENCE FROM SQLITE_SEQUENCE ' +
                                   'WHERE NAME = ''%s''', [ACommandInsert.Sequence.Name]));
  if Result = 0 then
  begin
    LSQL := Format('INSERT INTO SQLITE_SEQUENCE (NAME, SEQ) VALUES (''%s'', 0)',
                   [ACommandInsert.Sequence.Name]);
    FConnection.ExecuteDirect(LSQL);
  end;
end;

function TDMLGeneratorSQLite.GeneratorSequenceNextValue(AObject: TObject;
  ACommandInsert: TDMLCommandInsert): Int64;
var
  LSQL: String;
begin
  Result := GeneratorSequenceCurrentValue(AObject, ACommandInsert);
  LSQL := Format('UPDATE SQLITE_SEQUENCE SET SEQ = SEQ + %s WHERE NAME = ''%s''',
                 [IntToStr(ACommandInsert.Sequence.Increment), ACommandInsert.Sequence.Name]);
  FConnection.ExecuteDirect(LSQL);
  Result := Result + ACommandInsert.Sequence.Increment;
end;

initialization
  TDriverRegister.RegisterDriver(dnSQLite, TDMLGeneratorSQLite.Create);

end.

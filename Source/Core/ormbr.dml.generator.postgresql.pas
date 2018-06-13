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

unit ormbr.dml.generator.postgresql;

interface

uses
  SysUtils,
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
  /// Classe de banco de dados PostgreSQL
  /// </summary>
  TDMLGeneratorPostgreSQL = class(TDMLGeneratorAbstract)
  public
    constructor Create; override;
    destructor Destroy; override;
    function GeneratorSelectAll(AClass: TClass; APageSize: Integer; AID: Variant): string; override;
    function GeneratorSelectWhere(AClass: TClass; AWhere: string; AOrderBy: string; APageSize: Integer): string; override;
    function GeneratorSequenceCurrentValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64; override;
    function GeneratorSequenceNextValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64; override;
  end;

implementation

{ TDMLGeneratorPostgreSQL }

constructor TDMLGeneratorPostgreSQL.Create;
begin
  inherited;
  FDateFormat := 'yyyy-MM-dd';
  FTimeFormat := 'HH:MM:SS';
end;

destructor TDMLGeneratorPostgreSQL.Destroy;
begin

  inherited;
end;

function TDMLGeneratorPostgreSQL.GeneratorSelectAll(AClass: TClass; APageSize: Integer; AID: Variant): string;
var
  oCriteria: ICriteria;
begin
  oCriteria := GetCriteriaSelect(AClass, AID);
  if APageSize > -1 then
     Result := oCriteria.AsString + ' LIMIT %s OFFSET %s'
  else
     Result := oCriteria.AsString;
end;

function TDMLGeneratorPostgreSQL.GeneratorSelectWhere(AClass: TClass;
  AWhere: string; AOrderBy: string; APageSize: Integer): string;
var
  oCriteria: ICriteria;
begin
  oCriteria := GetCriteriaSelect(AClass, -1);
  oCriteria.Where(AWhere);
  oCriteria.OrderBy(AOrderBy);
  if APageSize > -1 then
     Result := oCriteria.AsString + ' LIMIT %s OFFSET %s'
  else
     Result := oCriteria.AsString;
end;

function TDMLGeneratorPostgreSQL.GeneratorSequenceCurrentValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64;
begin
  Result := ExecuteSequence(Format('SELECT CURRVAL(''%s'')', [ACommandInsert.Sequence.Name]));
end;

function TDMLGeneratorPostgreSQL.GeneratorSequenceNextValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64;
begin
  Result := ExecuteSequence(Format('SELECT NEXTVAL(''%s'')', [ACommandInsert.Sequence.Name]));
end;

initialization
  TDriverRegister.RegisterDriver(dnPostgreSQL, TDMLGeneratorPostgreSQL.Create);

end.


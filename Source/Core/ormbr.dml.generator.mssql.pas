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
  SysUtils,
  StrUtils,
  Rtti,
  ormbr.dml.generator,
  ormbr.mapping.classes,
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
    function GetGeneratorSelect(ACriteria: ICriteria): string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GeneratorSelectAll(AClass: TClass; APageSize: Integer; AID: Variant): string; override;
    function GeneratorSelectWhere(AClass: TClass; AWhere: string; AOrderBy: string; APageSize: Integer): string; override;
    function GeneratorSequenceCurrentValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64; override;
    function GeneratorSequenceNextValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64; override;
    function GeneratorPageNext(ACommandSelect: string; APageSize, APageNext: Integer): string; override;
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

function TDMLGeneratorMSSql.GetGeneratorSelect(ACriteria: ICriteria): string;
var
  LTable: string;
begin
   inherited;
   LTable := ACriteria.AST.Select.TableNames.Columns[0].Name;
   ACriteria.SelectSection(secSelect);
   ACriteria.Column('ROW_NUMBER() OVER(ORDER BY CURRENT_TIMESTAMP) AS ROWNUMBER');
   ACriteria.AST.Select.TableNames.Clear;
   ACriteria.From(LTable + ')').&As(LTable);
   ACriteria.SelectSection(secWhere);
   ACriteria.Where('(ROWNUMBER <= %s) AND (ROWNUMBER > %s)');
   ACriteria.SelectSection(secOrderBy);
   Result := 'SELECT * FROM (' + ACriteria.AsString;
end;

function TDMLGeneratorMSSql.GeneratorPageNext(ACommandSelect: string;
  APageSize, APageNext: Integer): string;
begin
  if APageSize > -1 then
     Result := Format(ACommandSelect, [IntToStr(APageNext + APageSize), IntToStr(APageNext)]);
end;

function TDMLGeneratorMSSql.GeneratorSelectAll(AClass: TClass; APageSize: Integer; AID: Variant): string;
var
  LCriteria: ICriteria;
begin
   LCriteria := GetCriteriaSelect(AClass, AID);
   if APageSize > -1 then
      Result := GetGeneratorSelect(LCriteria)
   else
      Result := LCriteria.AsString;
end;

function TDMLGeneratorMSSql.GeneratorSelectWhere(AClass: TClass; AWhere: string;
  AOrderBy: string; APageSize: Integer): string;
var
  LCriteria: ICriteria;
begin
  LCriteria := GetCriteriaSelect(AClass, -1);
  LCriteria.Where(AWhere);
  LCriteria.OrderBy(AOrderBy);
  if APageSize > -1 then
     Result := GetGeneratorSelect(LCriteria)
  else
     Result := LCriteria.AsString;
end;

function TDMLGeneratorMSSql.GeneratorSequenceCurrentValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64;
begin
  Result := ExecuteSequence(Format('SELECT CURRENT_VALUE FROM SYS.SEQUENCES WHERE NAME = ''%s''', [ACommandInsert.Sequence.Name]));
end;

function TDMLGeneratorMSSql.GeneratorSequenceNextValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64;
begin
  Result := ExecuteSequence(Format('SELECT NEXT VALUE FOR %s ', [ACommandInsert.Sequence.Name]));
end;

initialization
  TDriverRegister.RegisterDriver(dnMSSQL, TDMLGeneratorMSSql.Create);

end.

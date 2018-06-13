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

unit ormbr.dml.generator.firebird;

interface

uses
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
  /// Classe de banco de dados Firebird
  /// </summary>
  TDMLGeneratorFirebird = class(TDMLGeneratorAbstract)
  protected
    function GetGeneratorSelect(ACriteria: ICriteria): string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GeneratorSelectAll(AClass: TClass; APageSize: Integer;
      AID: Variant): string; override;
    function GeneratorSelectWhere(AClass: TClass; AWhere: string; AOrderBy: string;
      APageSize: Integer): string; override;
    function GeneratorSequenceCurrentValue(AObject: TObject;
      ACommandInsert: TDMLCommandInsert): Int64; override;
    function GeneratorSequenceNextValue(AObject: TObject;
      ACommandInsert: TDMLCommandInsert): Int64; override;
  end;

implementation

{ TDMLGeneratorFirebird }

constructor TDMLGeneratorFirebird.Create;
begin
  inherited;
  FDateFormat := 'MM/dd/yyyy';
  FTimeFormat := 'HH:MM:SS';
end;

destructor TDMLGeneratorFirebird.Destroy;
begin

  inherited;
end;

function TDMLGeneratorFirebird.GetGeneratorSelect(ACriteria: ICriteria): string;
begin
  inherited;
  ACriteria.AST.Select.Columns.Columns[0].Name := 'FIRST %s SKIP %s ' +
                                                  ACriteria.AST.Select.Columns.Columns[0].Name;
  Result := ACriteria.AsString;
end;

function TDMLGeneratorFirebird.GeneratorSelectAll(AClass: TClass;
  APageSize: Integer; AID: Variant): string;
var
  LCriteria: ICriteria;
begin
  LCriteria := GetCriteriaSelect(AClass, AID);
  if APageSize > -1 then
     Result := GetGeneratorSelect(LCriteria)
  else
     Result := LCriteria.AsString;
end;

function TDMLGeneratorFirebird.GeneratorSelectWhere(AClass: TClass;
  AWhere: string; AOrderBy: string; APageSize: Integer): string;
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

function TDMLGeneratorFirebird.GeneratorSequenceCurrentValue(AObject: TObject;
  ACommandInsert: TDMLCommandInsert): Int64;
begin
  Result := ExecuteSequence(
    Format('SELECT GEN_ID(%s, 0) FROM RDB$DATABASE;', [ACommandInsert.Sequence.Name]));
end;

function TDMLGeneratorFirebird.GeneratorSequenceNextValue(AObject: TObject;
  ACommandInsert: TDMLCommandInsert): Int64;
begin
  Result := ExecuteSequence(
    Format('SELECT GEN_ID(%s, %s) FROM RDB$DATABASE;', [ACommandInsert.Sequence.Name,
      IntToStr(ACommandInsert.Sequence.Increment)]));
end;

initialization
  TDriverRegister.RegisterDriver(dnFirebird, TDMLGeneratorFirebird.Create);

end.

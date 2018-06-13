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

unit ormbr.dml.generator.oracle;

interface

uses
  SysUtils,
  Rtti,
  ormbr.dml.generator,
  ormbr.mapping.classes,
  ormbr.mapping.explorer,
  ormbr.factory.interfaces,
  ormbr.types.database,
  ormbr.driver.register,
  ormbr.dml.commands,
  ormbr.criteria;

type
  /// <summary>
  /// Classe de conexão concreta com dbExpress
  /// </summary>
  TDMLGeneratorOracle = class(TDMLGeneratorAbstract)
  public
    constructor Create; override;
    destructor Destroy; override;
    function GeneratorSelectAll(AClass: TClass; APageSize: Integer; AID: Variant): string; override;
    function GeneratorSelectWhere(AClass: TClass; AWhere: string; AOrderBy: string; APageSize: Integer): string; override;
    function GeneratorSequenceCurrentValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64; override;
    function GeneratorSequenceNextValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64; override;
    function GeneratorPageNext(ACommandSelect: string; APageSize: Integer; APageNext: Integer): string; override;
  end;

implementation

const
  cSelectRow = 'SELECT * FROM ( ' + sLineBreak +
               '   SELECT T.*, ROWNUM ROWINI FROM (%s) T';

{ TDMLGeneratorOracle }

constructor TDMLGeneratorOracle.Create;
begin
  inherited;
  FDateFormat := 'yyyy-MM-dd';
  FTimeFormat := 'HH:MM:SS';
end;

destructor TDMLGeneratorOracle.Destroy;
begin
  inherited;
end;

function TDMLGeneratorOracle.GeneratorPageNext(ACommandSelect: string;
  APageSize: Integer; APageNext: Integer): string;
begin
  if APageSize > -1 then
     Result := Format(ACommandSelect, [IntToStr(APageNext + APageSize), IntToStr(APageNext)]);
end;

function TDMLGeneratorOracle.GeneratorSelectAll(AClass: TClass; APageSize: Integer; AID: Variant): string;
var
  oCriteria: ICriteria;
begin
  oCriteria := GetCriteriaSelect(AClass, AID);
  if APageSize > -1 then
  begin
     Result := Format(cSelectRow, [oCriteria.AsString]);
     Result := Result + sLineBreak +
               '   WHERE ROWNUM <= %s) ' + sLineBreak +
               'WHERE ROWINI > %s';
  end
  else
     Result := oCriteria.AsString;
end;

function TDMLGeneratorOracle.GeneratorSelectWhere(AClass: TClass; AWhere: string;
  AOrderBy: string; APageSize: Integer): string;
var
  oCriteria: ICriteria;
begin
  oCriteria := GetCriteriaSelect(AClass, -1);
  oCriteria.Where(AWhere);
  oCriteria.OrderBy(AOrderBy);
  if APageSize > -1 then
  begin
     Result := Format(cSelectRow, [oCriteria.AsString]);
     Result := Result + sLineBreak +
               '   WHERE ROWNUM <= %s) ' + sLineBreak +
               'WHERE ROWINI >= %s';
  end
  else
     Result := oCriteria.AsString;
end;

function TDMLGeneratorOracle.GeneratorSequenceCurrentValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64;
begin
  Result := ExecuteSequence(Format('SELECT %s.CURRVAL FROM DUAL', [ACommandInsert.Sequence.Name]));
end;

function TDMLGeneratorOracle.GeneratorSequenceNextValue(AObject: TObject; ACommandInsert: TDMLCommandInsert): Int64;
begin
  Result := ExecuteSequence(Format('SELECT %s.NEXTVAL FROM DUAL', [ACommandInsert.Sequence.Name]));
end;

initialization
  TDriverRegister.RegisterDriver(dnOracle, TDMLGeneratorOracle.Create);

end.


{
         CQL Brasil - Criteria Query Language for Delphi/Lazarus


                   Copyright (c) 2019, Isaque Pinheiro
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

{ @abstract(CQLBr Framework)
  @created(18 Jul 2019)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @author(Site : https://www.isaquepinheiro.com.br)
}

unit cqlbr.select.oracle;

interface

uses
  SysUtils,
  cqlbr.select;

type
  TCQLSelectOracle = class(TCQLSelect)
  public
    constructor Create; override;
    function Serialize: String; override;
  end;

implementation

uses
  cqlbr.utils,
  cqlbr.interfaces,
  cqlbr.db.register,
  cqlbr.qualifier.oracle;

{ TCQLSelectOracle }

constructor TCQLSelectOracle.Create;
begin
  inherited;
  FQualifiers := TCQLSelectQualifiersOracle.New;
end;

function TCQLSelectOracle.Serialize: String;
begin
  if IsEmpty then
    Result := ''
  else
    Result := TUtils.Concat(['SELECT',
                             FColumns.Serialize,
                             FQualifiers.SerializeDistinct,
                             'FROM',
                             FTableNames.Serialize]);
end;

initialization
  TDBRegister.RegisterSelect(dbnOracle, TCQLSelectOracle.Create);

end.

{
         CQL Brasil - Criteria Query Language for Delphi/Lazarus


                   Copyright (c) 2019, Isaque Pinheiro
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

{ @abstract(CQLBr Framework)
  @created(18 Jul 2019)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @author(Site : https://www.isaquepinheiro.com.br)
}

unit cqlbr.select.mssql;

interface

uses
  SysUtils,
  cqlbr.interfaces,
  cqlbr.select;

type
  TCQLSelectMSSQL = class(TCQLSelect)
  public
    constructor Create; override;
    function Serialize: String; override;
  end;

implementation

uses
  cqlbr.utils,
  cqlbr.db.register,
  cqlbr.qualifier.mssql;

{ TCQLSelectMSSQL }

constructor TCQLSelectMSSQL.Create;
begin
  inherited Create;
  FQualifiers := TCQLSelectQualifiersMSSQL.New;
end;

function TCQLSelectMSSQL.Serialize: String;
var
  LFor: Integer;

  function DoSerialize: String;
  begin
    Result := TUtils.Concat(['SELECT',
                             FColumns.Serialize,
                             FQualifiers.SerializeDistinct,
                             'FROM',
                             FTableNames.Serialize]);
  end;

const
  cSELECT = 'SELECT * FROM (%s) AS %s';

begin
  if IsEmpty then
    Result := ''
  else
  begin
    if FQualifiers.ExecutingPagination then
    begin
      FColumns.Add.Name := 'ROW_NUMBER() OVER(ORDER BY CURRENT_TIMESTAMP) AS ROWNUMBER';
      Result := Format(cSELECT, [DoSerialize, FTableNames.Serialize]);
    end
    else
      Result := DoSerialize;
  end;
end;

initialization
  TDBRegister.RegisterSelect(dbnMSSQL, TCQLSelectMSSQL.Create);

end.

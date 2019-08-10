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

unit cqlbr.qualifier.oracle;

interface

uses
  SysUtils,
  cqlbr.interfaces,
  cqlbr.qualifier;

type
  TCQLSelectQualifiersOracle = class(TCQLSelectQualifiers)
  public
    function SerializePagination: String; override;
    class function New: TCQLSelectQualifiersOracle;
  end;

implementation

uses
  cqlbr.utils;

{ TCQLSelectQualifiersOracle }

class function TCQLSelectQualifiersOracle.New: TCQLSelectQualifiersOracle;
begin
  Result := Self.Create;
end;

function TCQLSelectQualifiersOracle.SerializePagination: String;
var
  LFor: Integer;
  LFirst: String;
  LSkip: String;
begin
  Result := 'SELECT * FROM (SELECT T.*, ROWNUM AS ROWINI FROM (%s) T)';
  for LFor := 0 to Count -1 do
  begin
    case FQualifiers[LFor].Qualifier of
      sqFirst: LFirst := TUtils.Concat(['WHERE ROWNUM <=', IntToStr(FQualifiers[LFor].Value)]);
      sqSkip:  LSkip  := TUtils.Concat(['AND ROWINI >', IntToStr(FQualifiers[LFor].Value)]);
    else
      raise Exception.Create('TCQLSelectQualifiersOracle.SerializeSelectQualifiers: Unknown qualifier');
    end;
  end;
  Result := TUtils.Concat([Result, LFirst, LSkip]);
end;

end.

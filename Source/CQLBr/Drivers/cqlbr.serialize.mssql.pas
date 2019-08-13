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

unit cqlbr.serialize.mssql;

interface

uses
  SysUtils,
  cqlbr.utils,
  cqlbr.db.register,
  cqlbr.interfaces,
  cqlbr.serialize;

type
  TCQLSerializerMSSQL = class(TCQLSerialize)
  public
    function AsString(const AAST: ICQLAST): String; override;
  end;

implementation

{ TCQLSerializer }

function TCQLSerializerMSSQL.AsString(const AAST: ICQLAST): String;
var
  LWhere: String;
begin
  LWhere := AAST.Where.Serialize;
  /// <summary>
  ///   Gera sintaxe para caso exista comando de pagina��o.
  /// </summary>
  if AAST.Select.Qualifiers.ExecutingPagination then
  begin
    if LWhere = '' then
      LWhere := TUtils.Concat(['WHERE', '(' + AAST.Select.Qualifiers.SerializePagination + ')'])
    else
      LWhere := TUtils.Concat([Result, 'AND', '(' + AAST.Select.Qualifiers.SerializePagination + ')']);
  end;
  Result := TUtils.Concat([AAST.Select.Serialize,
                           AAST.Delete.Serialize,
                           AAST.Insert.Serialize,
                           AAST.Update.Serialize,
                           AAST.Joins.Serialize,
                           LWhere,
                           AAST.GroupBy.Serialize,
                           AAST.Having.Serialize,
                           AAST.OrderBy.Serialize]);
end;

initialization
  TDBRegister.RegisterSerialize(dbnMSSQL, TCQLSerializerMSSQL.Create);

end.

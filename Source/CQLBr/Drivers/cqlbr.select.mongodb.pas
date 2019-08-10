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

unit cqlbr.select.mongodb;

interface

uses
  SysUtils,
  cqlbr.select;

type
  TCQLSelectMongoDB = class(TCQLSelect)
  public
    constructor Create; override;
    function Serialize: String; override;
  end;

implementation


uses
  cqlbr.utils,
  cqlbr.db.register,
  cqlbr.interfaces,
  cqlbr.qualifier.mongodb;

{ TCQLSelectMongoDB }

constructor TCQLSelectMongoDB.Create;
begin
  inherited;
  FQualifiers := TCQLSelectQualifiersMongodb.New;
end;

function TCQLSelectMongoDB.Serialize: String;
begin
  if IsEmpty then
    Result := ''
  else
  begin
    Result := FTableNames.Serialize + '.find( {'
            + FColumns.Serialize + '} )';
    Result := LowerCase(Result);
  end;
//                             FQualifiers.SerializeDistinct,
//                             FQualifiers.SerializePagination,
end;

initialization
  TDBRegister.RegisterSelect(dbnMongoDB, TCQLSelectMongoDB.Create);

end.

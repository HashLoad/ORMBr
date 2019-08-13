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

unit cqlbr.serialize.firebird;

interface

uses
  SysUtils,
  cqlbr.db.register,
  cqlbr.interfaces,
  cqlbr.serialize;

type
  TCQLSerializerFirebird = class(TCQLSerialize)
  public
    function AsString(const AAST: ICQLAST): String; override;
  end;

implementation

{ TCQLSerializer }

function TCQLSerializerFirebird.AsString(const AAST: ICQLAST): String;
begin
  Result := inherited AsString(AAST);
end;

initialization
  TDBRegister.RegisterSerialize(dbnFirebird, TCQLSerializerFirebird.Create);

end.


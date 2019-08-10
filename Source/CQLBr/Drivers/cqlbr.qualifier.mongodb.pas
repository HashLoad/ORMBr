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

unit cqlbr.qualifier.mongodb;

interface

uses
  SysUtils,
  cqlbr.interfaces,
  cqlbr.qualifier;

type
  TCQLSelectQualifiersMongoDB = class(TCQLSelectQualifiers)
  public
    function SerializePagination: String; override;
    class function New: TCQLSelectQualifiersMongoDB;
  end;

implementation

uses
  cqlbr.utils;

{ TCQLSelectQualifiersMongoDB }

class function TCQLSelectQualifiersMongoDB.New: TCQLSelectQualifiersMongoDB;
begin
  Result := Self.Create;
end;

function TCQLSelectQualifiersMongoDB.SerializePagination: String;
var
  LFor: Integer;
  LFirst: String;
  LSkip: String;
begin
  Result := '';
//  for LFor := 0 to Count -1 do
//  begin
//    case FQualifiers[LFor].Qualifier of
//      sqFirst: LFirst := TUtils.Concat(['FIRST', IntToStr(FQualifiers[LFor].Value)]);
//      sqSkip:  LSkip  := TUtils.Concat(['SKIP' , IntToStr(FQualifiers[LFor].Value)]);
//    else
//      raise Exception.Create('TCQLSelectQualifiersFirebird.SerializeSelectQualifiers: Unknown qualifier');
//    end;
//  end;
//  Result := TUtils.Concat([Result, LFirst, LSkip]);
end;

end.

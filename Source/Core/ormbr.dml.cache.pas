{
      ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
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

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)
}

unit ormbr.dml.cache;

interface

uses
  Generics.Collections;

type
  TQueryCache = class
  private
    class var FQueryCache: TDictionary<String, String>;
  public
    class constructor Create;
    class destructor Destroy;
    /// <summary>
    ///   Lista para cache de comandos SQL, evitando o ORMBr usar RTTi toda
    ///   vez que for solicitado um SELECT e INSERT.
    /// </summary>
    /// <param name="String">
    ///   Key de localiza��o por classe e comando
    /// </param>
    /// <param name="String">
    ///   Comando SQL pronto para SELECT e INSERT
    /// </param>
    class function Get: TDictionary<String, String>;
  end;

implementation

{ TQueryCache }

class constructor TQueryCache.Create;
begin
  FQueryCache := TDictionary<String, String>.Create;
end;

class destructor TQueryCache.Destroy;
begin
  FQueryCache.Free;
end;

class function TQueryCache.Get: TDictionary<String, String>;
begin
  Result := FQueryCache;
end;

end.

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
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)
}

unit ormbr.dml.cache;

interface

uses
  Generics.Collections;

type
  TDMLCache = class
  private
    class var FDMLCache: TDictionary<String, String>;
  public
    class constructor Create;
    class destructor Destroy;
    /// <summary>
    ///   Lista para cache de comandos SQL, evitando o ORMBr usar RTTi toda
    ///   vez que for solicitado um SELECT e INSERT.
    /// </summary>
    /// <param name="String">
    ///   Key de localização por classe e comando
    /// </param>
    /// <param name="String">
    ///   Comando SQL pronto para SELECT e INSERT
    /// </param>
    class function DMLCache: TDictionary<String, String>;
  end;

implementation

{ TDMLCache }

class constructor TDMLCache.Create;
begin
  FDMLCache := TDictionary<String, String>.Create;
end;

class destructor TDMLCache.Destroy;
begin
  FDMLCache.Free;
end;

class function TDMLCache.DMLCache: TDictionary<String, String>;
begin
  Result := FDMLCache;
end;

end.

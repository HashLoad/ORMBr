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

{$INCLUDE ..\ormbr.inc}

unit ormbr.register.middleware;

interface

uses
  SysUtils,
  Generics.Collections;

type
  TQueryScopeList = TDictionary<String, TFunc<String>>;
  TQueryScopeCallback = reference to function(const AResource: String): TQueryScopeList;

  TMiddleware = class
  private
    FQueryScopeCallback: TQueryScopeCallback;
  public
    constructor Create(const ACallback: TQueryScopeCallback); overload;
    function ExecuteQueryScopeCallback(const AResource: String): TQueryScopeList;
  end;

  TORMBrMiddlewares = class
  private
    class var FQueryScopeCallbacks: TDictionary<String, TMiddleware>;
  public
    class constructor Create;
    class destructor Destroy;
    // Query Scope
    class procedure RegisterQueryScopeCallback(const ANameCallback: String;
      const ACallback: TQueryScopeCallback);
    class function ExecuteQueryScopeCallback(const AClass: TClass;
      const ANameCallback: String): TQueryScopeList;
  end;

implementation

{ TORMBrMiddleware }

class constructor TORMBrMiddlewares.Create;
begin
  FQueryScopeCallbacks := TObjectDictionary<String, TMiddleware>.Create([doOwnsValues]);
end;

class destructor TORMBrMiddlewares.Destroy;
begin
  FQueryScopeCallbacks.Free;
end;

class procedure TORMBrMiddlewares.RegisterQueryScopeCallback(const ANameCallback: String;
  const ACallback: TQueryScopeCallback);
begin
  FQueryScopeCallbacks.AddOrSetValue(ANameCallback, TMiddleware.Create(ACallback));
end;

class function TORMBrMiddlewares.ExecuteQueryScopeCallback(const AClass: TClass;
  const ANameCallback: String): TQueryScopeList;
begin
  Result := nil;
  if not FQueryScopeCallbacks.ContainsKey(ANameCallback) then
    Exit;
  Result := FQueryScopeCallbacks[ANameCallback].ExecuteQueryScopeCallback(UpperCase(AClass.ClassName));
end;

{ TQueryScopeMiddleware }

constructor TMiddleware.Create(const ACallback: TQueryScopeCallback);
begin
  FQueryScopeCallback := ACallback;
end;

function TMiddleware.ExecuteQueryScopeCallback(const AResource: String): TQueryScopeList;
begin
  Result := FQueryScopeCallback(AResource);
end;

end.

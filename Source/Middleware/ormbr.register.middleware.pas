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
  TORMBrEventType = (onBeforeInsert, onAfeterInsert,
                     onBeforeUpdate, onAfterUpdate,
                     onBeforeDelete, onAfterDelete);

  TQueryScopeList = TDictionary<String, TFunc<String>>;
  TQueryScopeCallback = reference to function(const AResource: String): TQueryScopeList;
  TEvent = TProc<TObject>;
  TEventCallback = reference to function(const AResource: String): TEvent;

  TMiddlewareQueryScope = class
  private
    FQueryScopeCallback: TQueryScopeCallback;
  public
    constructor Create(const ACallback: TQueryScopeCallback); overload;
    function ExecuteQueryScopeCallback(const AResource: String): TQueryScopeList;
  end;

  TMiddlewareEvent = class
  private
    FEventCallback: TEventCallback;
  public
    constructor Create(const ACallback: TEventCallback); overload;
    function ExecuteEventCallback(const AResource: String): TEvent;
  end;

  TORMBrMiddlewares = class
  private
    class var FQueryScopeCallbacks: TDictionary<String, TMiddlewareQueryScope>;
    class var FEventCallbacks: TDictionary<String, TMiddlewareEvent>;
  public
    class constructor Create;
    class destructor Destroy;
    // Query Scope
    class procedure RegisterQueryScopeCallback(const ANameCallback: String;
      const ACallback: TQueryScopeCallback);
    class function ExecuteQueryScopeCallback(const AClass: TClass;
      const ANameCallback: String): TQueryScopeList;
    // Events
    class procedure RegisterEventCallback(const ANameCallback: String;
      const ACallback: TEventCallback);
    class function ExecuteEventCallback(const AClass: TClass;
      const ANameCallback: String): TEvent;
  end;

implementation

{ TORMBrMiddleware }

class constructor TORMBrMiddlewares.Create;
begin
  FQueryScopeCallbacks := TObjectDictionary<String, TMiddlewareQueryScope>.Create([doOwnsValues]);
  FEventCallbacks := TObjectDictionary<String, TMiddlewareEvent>.Create([doOwnsValues]);
end;

class destructor TORMBrMiddlewares.Destroy;
begin
  FQueryScopeCallbacks.Free;
  FEventCallbacks.Free;
end;

class procedure TORMBrMiddlewares.RegisterEventCallback(
  const ANameCallback: String; const ACallback: TEventCallback);
begin
  FEventCallbacks.AddOrSetValue(ANameCallback, TMiddlewareEvent.Create(ACallback));
end;

class procedure TORMBrMiddlewares.RegisterQueryScopeCallback(const ANameCallback: String;
  const ACallback: TQueryScopeCallback);
begin
  FQueryScopeCallbacks.AddOrSetValue(ANameCallback, TMiddlewareQueryScope.Create(ACallback));
end;

class function TORMBrMiddlewares.ExecuteEventCallback(const AClass: TClass;
  const ANameCallback: String): TEvent;
begin
  Result := nil;
  if not FEventCallbacks.ContainsKey(ANameCallback) then
    Exit;
  Result := FEventCallbacks[ANameCallback].ExecuteEventCallback(UpperCase(AClass.ClassName));
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

constructor TMiddlewareQueryScope.Create(const ACallback: TQueryScopeCallback);
begin
  FQueryScopeCallback := ACallback;
end;

function TMiddlewareQueryScope.ExecuteQueryScopeCallback(const AResource: String): TQueryScopeList;
begin
  Result := FQueryScopeCallback(AResource);
end;

{ TMiddlewareEvent }

constructor TMiddlewareEvent.Create(const ACallback: TEventCallback);
begin
  FEventCallback := ACallback;
end;

function TMiddlewareEvent.ExecuteEventCallback(const AResource: String): TEvent;
begin
  Result := FEventCallback(AResource);
end;

end.

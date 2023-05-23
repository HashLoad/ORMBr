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

unit ormbr.query.scope.middleware;

interface

uses
  SysUtils,
  Generics.Collections,
  ormbr.register.middleware;

type
  IQueryScopeMiddleware = interface
    ['{57DDFB1C-B262-4B8C-BC54-7B9827ACAE38}']
    procedure AddWhere(const AResource, AScopeName: String; const AFunc: TFunc<String>);
    procedure AddOrderBy(const AResource, AScopeName: String; const AFunc: TFunc<String>);
  end;

  TQueryScopeMiddleware = class(TInterfacedObject, IQueryScopeMiddleware)
  strict private
    class var FInstance: IQueryScopeMiddleware;
    class var FScopeWhereList: TDictionary<String, TQueryScopeList>;
    class var FScopeOrderByList: TDictionary<String, TQueryScopeList>;
    constructor CreatePrivate;
  protected
    constructor Create;
  public
    destructor Destroy; override;
    class function Get: IQueryScopeMiddleware;
    procedure AddWhere(const AResource, AScopeName: String; const AFunc: TFunc<String>);
    procedure AddOrderBy(const AResource, AScopeName: String; const AFunc: TFunc<String>);
    class function GetWhere(const AResource: String): TQueryScopeList;
    class function GetOrderBy(const AResource: String): TQueryScopeList;
  end;

function QueryScopeMiddleware: IQueryScopeMiddleware;

implementation

function QueryScopeMiddleware: IQueryScopeMiddleware;
begin
  Result := TQueryScopeMiddleware.Get;
end;

{ TQueryScope }

procedure TQueryScopeMiddleware.AddOrderBy(const AResource, AScopeName: String;
  const AFunc: TFunc<String>);
var
  LResource: String;
  LScopeName: String;
begin
  LResource := UpperCase(AResource);
  LScopeName := UpperCase(AScopeName);
  if not FScopeOrderByList.ContainsKey(LResource) then
    FScopeOrderByList.Add(LResource, TQueryScopeList.Create);
  if not FScopeOrderByList[LResource].ContainsKey(LScopeName) then
    FScopeOrderByList[LResource].Add(LScopeName, AFunc);
end;

procedure TQueryScopeMiddleware.AddWhere(const AResource, AScopeName: String;
  const AFunc: TFunc<String>);
var
  LResource: String;
  LScopeName: String;
begin
  LResource := UpperCase(AResource);
  LScopeName := UpperCase(AScopeName);
  if not FScopeWhereList.ContainsKey(LResource) then
    FScopeWhereList.Add(LResource, TQueryScopeList.Create);
  if not FScopeWhereList[LResource].ContainsKey(LScopeName) then
    FScopeWhereList[LResource].Add(LScopeName, AFunc);
end;

constructor TQueryScopeMiddleware.Create;
begin
  raise Exception.Create('Para usar o IQueryScopeMiddleware chame QueryScopeMiddleware.');
end;

constructor TQueryScopeMiddleware.CreatePrivate;
begin
  FScopeWhereList := TObjectDictionary<String, TQueryScopeList>.Create([doOwnsValues]);
  FScopeOrderByList := TObjectDictionary<String, TQueryScopeList>.Create([doOwnsValues]);
end;

destructor TQueryScopeMiddleware.Destroy;
begin
  FScopeWhereList.Free;
  FScopeOrderByList.Free;
  inherited;
end;

class function TQueryScopeMiddleware.Get: IQueryScopeMiddleware;
begin
  if not Assigned(FInstance) then
    FInstance := TQueryScopeMiddleware.CreatePrivate;
   Result := FInstance;
end;

class function TQueryScopeMiddleware.GetOrderBy(const AResource: String): TQueryScopeList;
begin
  Result := nil;
  if not FScopeOrderByList.ContainsKey(AResource) then
    Exit;
  Result := FScopeOrderByList[AResource];
end;

class function TQueryScopeMiddleware.GetWhere(const AResource: String): TQueryScopeList;
begin
  Result := nil;
  if not FScopeWhereList.ContainsKey(AResource) then
    Exit;
  Result := FScopeWhereList[AResource];
end;

initialization
  TORMBrMiddlewares.RegisterQueryScopeCallback('GetWhere', TQueryScopeMiddleware.GetWhere);
  TORMBrMiddlewares.RegisterQueryScopeCallback('GetOrderBy', TQueryScopeMiddleware.GetOrderBy);

end.

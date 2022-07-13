{
      ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2018, Isaque Pinheiro
                          All rights reserved.
}

{ 
  @abstract(REST Componentes)
  @created(20 Jun 2018)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)
}

unit ormbr.server.resource.wirl;

interface

uses
  SysUtils,
  // WiRL
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  // ORMBr
  ormbr.rest.query.parse,
  ormbr.server.resource;

type
  [Path('/ormbr')]
  TAppResource = class(TAppResourceBase)
  public
    constructor Create; overload; virtual;
    destructor Destroy; override;

    [GET, Path('/{resource}?')]
    [Produces(TMediaType.TEXT_PLAIN)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function select([PathParam] resource: String;
                    [QueryParam('$filter')] filter: String;
                    [QueryParam('$orderby')] orderby: String;
                    [QueryParam('$top')] top: String;
                    [QueryParam('$skip')] skip: String;
                    [QueryParam('$count')] count: String): string; overload;

    [POST, Path('/{resource}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function insert([PathParam] resource: String;
                    [BodyParam] value: String): String; overload;

    [PUT, Path('/{resource}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function update([PathParam] resource: String;
                    [BodyParam] value: String): String; overload;

    [DELETE, Path('/{resource}?')]
    [Produces(TMediaType.TEXT_PLAIN)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function delete([PathParam] resource: String;
                    [QueryParam('$filter')] filter: String): String; overload;
  end;

implementation

uses
  ormbr.server.wirl;

{ TAppResource }

constructor TAppResource.Create;
begin
  Create(TRESTServerWiRL.GetConnection);
end;

destructor TAppResource.Destroy;
begin

  inherited;
end;

function TAppResource.select(resource: String;
                             filter: String;
                             orderby: String;
                             top: String;
                             skip: String;
                             count: String): string;
var
  LQuery: TRESTQuery;
begin
  LQuery := TRESTQuery.Create;
  try
    // Parse da Query passada na URI
    LQuery.ParseQuery(resource);
    if LQuery.ResourceName <> '' then
    begin
      LQuery.SetFilter(filter);
      LQuery.SetOrderBy(orderby);
      LQuery.SetTop(top);
      LQuery.SetSkip(skip);
      LQuery.SetCount(count);
      // Retorno JSON
      Result := ParseFind(LQuery);
      // Add Count Record no JSON Result
//      if LQuery.Count then
    end
    else
      raise Exception.Create('Class ' + LQuery.ResourceName + 'not found!');
  finally
    LQuery.Free;
  end;
end;

function TAppResource.insert(resource: String; value: String): String;
begin
  Result := inherited;
end;

function TAppResource.update(resource: String; value: String): String;
begin
  Result := inherited;
end;

function TAppResource.delete(resource: String;
                             filter: String): String;
var
  LQuery: TRESTQuery;
begin
  LQuery := TRESTQuery.Create;
  try
    // Parse da Query passada na URI
    LQuery.ParseQuery(resource);
    if LQuery.ResourceName <> '' then
    begin
      LQuery.SetFilter(filter);
      // Retorno JSON
      Result := ParseDelete(LQuery);
    end
    else
      raise Exception.Create('Class ' + LQuery.ResourceName + 'not found!');
  finally
    LQuery.Free;
  end;
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TAppResource>;

end.

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

unit ormbr.server.resource.mars;

interface

uses
  SysUtils,
  // MARS
  MARS.Core.Registry,
  MARS.Core.Attributes,
  MARS.Core.MediaType,
  // JSON
  JSON,
  // ORMBr
  ormbr.rest.query.parse,
  ormbr.server.resource;

type
  [Path('/ormbr')]
  TAppResource = class
  private
    FAppResource: TAppResourceBase;
  public
    constructor Create;
    destructor Destroy; override;

    [GET, Path('/{resource}?')]
    [Produces(TMediaType.TEXT_PLAIN)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function select([PathParam] resource: String;
                    [QueryParam('$filter')] filter: String;
                    [QueryParam('$orderby')] orderby: String;
                    [QueryParam('$top')] top: String;
                    [QueryParam('$skip')] skip: String;
                    [QueryParam('$count')] count: String): TJSONValue; overload;

    [POST, Path('/{resource}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function insert([PathParam] resource: String;
                    [BodyParam] value: String): TJSONValue; overload;

    [PUT, Path('/{resource}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function update([PathParam] resource: String;
                    [BodyParam] value: String): TJSONValue; overload;

    [DELETE, Path('/{resource}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function delete([PathParam] resource: String;
                    [QueryParam('$filter')] filter: String): TJSONValue; overload;
  end;

implementation

uses
  ormbr.rest.json,
  ormbr.json.utils,
  ormbr.server.mars;

{ TAppResource }

constructor TAppResource.Create;
begin
  FAppResource := TAppResourceBase.Create(TRESTServerMARS.GetConnection);
end;

destructor TAppResource.Destroy;
begin
  FAppResource.Free;
  inherited;
end;

function TAppResource.select(resource: String;
                             filter: String;
                             orderby: String;
                             top: String;
                             skip: String;
                             count: String): TJSONValue;
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
      Result := TORMBrJSONUtil
                  .JSONStringToJSONValue(FAppResource.ParseFind(LQuery));
      // Add Count Record no JSON Result
//      if LQuery.Count then
    end
    else
      raise Exception.Create('Class ' + LQuery.ResourceName + 'not found!');
  finally
    LQuery.Free;
  end;
end;

function TAppResource.insert(resource: String; value: String): TJSONValue;
begin
  Result := TORMBrJSONUtil
              .JSONStringToJSONValue(FAppResource.insert(resource, value));
end;

function TAppResource.update(resource: String; value: String): TJSONValue;
begin
  Result := TORMBrJSONUtil
              .JSONStringToJSONValue(FAppResource.update(resource, value));
end;

function TAppResource.delete(resource: String;
                             filter: String): TJSONValue;
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
      Result := TORMBrJSONUtil
                  .JSONStringToJSONValue(FAppResource.ParseDelete(LQuery));
    end
    else
      raise Exception.Create('Class ' + LQuery.ResourceName + 'not found!');
  finally
    LQuery.Free;
  end;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TAppResource>;

end.

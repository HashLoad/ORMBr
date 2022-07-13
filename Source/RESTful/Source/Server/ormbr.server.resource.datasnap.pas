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

unit ormbr.server.resource.datasnap;

interface

uses
  Classes,
  SysUtils,
  /// DataSnap
  Datasnap.DSServer,
  Datasnap.DSAuth,
  Datasnap.DSReflect,
  Datasnap.DSCommonServer,
  Datasnap.DSNames,
  Data.DBXPlatform,
  JSON,
  /// ORMBr
  ormbr.rest.query.parse,
  ormbr.server.resource;

type
{$METHODINFO ON}
  ormbr = class(TComponent)
  private
    FAppResource: TAppResourceBase;
  protected
    const cDELIM_QUERY = '?';
    const cDELIM_AND = '&';
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function app(resource: String): TJSONValue;
    function acceptapp(resource: String; value: TJSONValue): TJSONValue;
    function updateapp(resource: String; value: TJSONValue): TJSONValue;
    function cancelapp(resource: String): TJSONValue;
  end;
{$METHODINFO OFF}

implementation

uses
  /// ORMBr JSON e DataSnap
  ormbr.rest.json,
  ormbr.json.utils,
  ormbr.server.datasnap;

{ TAppResource }

constructor ormbr.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAppResource := TAppResourceBase.Create(TRESTServerDataSnap.GetConnection);
end;

destructor ormbr.Destroy;
begin
  FAppResource.Free;
  inherited;
end;

function ormbr.app(resource: String): TJSONValue;
var
  LQuery: TRESTQuery;
  LQueryParams: TStringList;
  LQueryText: String;
  LFor: Integer;
begin
  LQuery := TRESTQuery.Create;
  LQueryParams := TStringList.Create;
  LQueryParams.Assign(GetInvocationMetadata().QueryParams);
  try
    //Monta a URL com a Query Param completa, necessário no parse interno
    LQueryText := resource + cDELIM_QUERY;
    for LFor := 0 to LQueryParams.Count -1 do
    begin
      LQueryText := LQueryText + LQueryParams.Strings[LFor];
      if LFor < LQueryParams.Count -1 then
        LQueryText := LQueryText + cDELIM_AND;
    end;
    // Parse da Query passada na URI
    LQuery.ParseQuery(LQueryText);
    if LQuery.ResourceName <> '' then
    begin
      // Retorno JSON
      Result := TORMBrJSONUtil
                  .JSONStringToJSONValue(FAppResource.ParseFind(LQuery));
      // Add Count Record no JSON Result
//      if LQuery.Count then
    end
    else
      raise Exception.Create('Class ' + LQuery.ResourceName + 'not found!');
  finally
    LQueryParams.Free;
    LQuery.Free;
  end;
end;

function ormbr.acceptapp(resource: String; value: TJSONValue): TJSONValue;
begin
  Result := TORMBrJSONUtil
              .JSONStringToJSONValue(FAppResource.insert(resource, value.ToJSON));
end;

function ormbr.updateapp(resource: String; value: TJSONValue): TJSONValue;
begin
  Result := TORMBrJSONUtil
              .JSONStringToJSONValue(FAppResource.update(resource, value.ToJSON));
end;

function ormbr.cancelapp(resource: String): TJSONValue;
var
  LQuery: TRESTQuery;
  LQueryParams: TStringList;
  LQueryText: String;
  LFor: Integer;
begin
  LQuery := TRESTQuery.Create;
  LQueryParams := TStringList.Create;
  LQueryParams.Assign(GetInvocationMetadata().QueryParams);
  try
    // Monta a URL com a Query Param completa, necessário no parse interno
    LQueryText := resource + cDELIM_QUERY;
    for LFor := 0 to LQueryParams.Count -1 do
    begin
      LQueryText := LQueryText + LQueryParams.Strings[LFor];
      if LFor < LQueryParams.Count -1 then
        LQueryText := LQueryText + cDELIM_AND;
    end;
    // Parse da Query passada na URI
    LQuery.ParseQuery(LQueryText);
    if LQuery.ResourceName <> '' then
    begin
      // Retorno JSON
      Result := TORMBrJSONUtil
                  .JSONStringToJSONValue(FAppResource.ParseDelete(LQuery));
    end
    else
      raise Exception.Create('Class ' + LQuery.ResourceName + 'not found!');
  finally
    LQueryParams.Free;
    LQuery.Free;
  end;
end;

end.

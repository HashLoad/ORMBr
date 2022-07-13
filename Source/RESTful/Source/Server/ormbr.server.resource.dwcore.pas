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

unit ormbr.server.resource.dwcore;

interface

uses
  Classes,
  SysUtils,
  /// DwCore
  uDWDatamodule,
  uDWJSONObject,
  uDWJSONTools,
  uDWConsts,
  uRESTDWServerEvents,
  JSON,
  /// ORMBr
  ormbr.rest.query.parse,
  ormbr.server.resource;

type
{$METHODINFO ON}
  TServerMethods = class(TServerMethodDataModule)
  private
    FAppResource: TAppResourceBase;
    FServerEvents: TDWServerEvents;
    procedure DoReplyEventByType(var Params: TDWParams; var Result: string;
      const RequestType: TRequestType; var StatusCode: Integer;
      RequestHeader: TStringList);
    function GetParamValue(const AValue: String): String;
    function select(const AParams: TDWParams): String;
    function insert(const AParams: TDWParams): String;
    function update(const AParams: TDWParams): String;
    function delete(const AParams: TDWParams): String;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
{$METHODINFO OFF}

implementation

uses
  /// ORMBr JSON
  ormbr.rest.json,
  ormbr.json.utils,
  ormbr.server.dwcore;

{ TAppResource }

constructor TServerMethods.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAppResource := TAppResourceBase.Create(TRESTServerDWCore.GetConnection);
  FServerEvents := TDWServerEvents.Create(Self);
  FServerEvents.Name := 'ServerEvents';
  FServerEvents.ContextName := 'ormbr';
  with FServerEvents.Events.Add as TDWEvent do
  begin
    JsonMode := jmPureJSON;
    Name := 'api';
    OnReplyEventByType := DoReplyEventByType;
  end;
end;

destructor TServerMethods.Destroy;
begin
  FServerEvents.Free;
  FAppResource.Free;
  inherited;
end;

procedure TServerMethods.DoReplyEventByType(
  var Params: TDWParams; var Result: string; const RequestType: TRequestType;
  var StatusCode: Integer; RequestHeader: TStringList);
var
  LParam: TJSONParam;
begin
  LParam := nil;
  try
    LParam := Params.ItemsString['requesttype'];
    if LParam = nil then
      raise Exception.Create('resource parameter not passed');

    if GetParamValue(LParam.AsString) = 'GET' then
      Result := select(Params)
    else
    if GetParamValue(LParam.AsString) = 'POST' then
      Result := insert(Params)
    else
    if GetParamValue(LParam.AsString) = 'PUT' then
      Result := update(Params)
    else
    if GetParamValue(LParam.AsString) = 'DELETE' then
      Result := delete(Params);
  except
    on E: Exception do
    begin
      Result := 'Exception: ' + E.Message;
    end;
  end;

//  case RequestType of
//    rtGet:    Result := select(Params);
//    rtPost:   Result := insert(Params);
//    rtPut:    Result := update(Params);
//    rtDelete: Result := delete(Params);
//    rtPatch:  Result := '';
//  end;
end;

function TServerMethods.select(const AParams: TDWParams): String;
var
  LAppResource: TAppResourceBase;
  LQuery: TRESTQuery;
  LParam: TJSONParam;
begin
  LParam := nil;
  LParam := AParams.ItemsString['resource'];
  if LParam = nil then
    raise Exception.Create('resource parameter not passed');
  LQuery := TRESTQuery.Create;
  LAppResource := TAppResourceBase.Create(TRESTServerDWCore.GetConnection);
  try
    // Parse da Query passada na URI
    LQuery.ParseQuery(GetParamValue(LParam.AsString));
    if LQuery.ResourceName <> '' then
    begin
      LParam := nil;
      LParam := AParams.ItemsString['$filter'];
      if LParam <> nil then
        LQuery.SetFilter(GetParamValue(LParam.AsString));
      LParam := nil;
      LParam := AParams.ItemsString['$orderby'];
      if LParam <> nil then
        LQuery.SetOrderBy(GetParamValue(LParam.AsString));
      LParam := nil;
      LParam := AParams.ItemsString['$top'];
      if LParam <> nil then
        LQuery.SetTop(GetParamValue(LParam.AsString));
      LParam := nil;
      LParam := AParams.ItemsString['$skip'];
      if LParam <> nil then
        LQuery.SetSkip(GetParamValue(LParam.AsString));
      LParam := nil;
      LParam := AParams.ItemsString['$count'];
      if LParam <> nil then
        LQuery.SetCount(GetParamValue(LParam.AsString));
      // Retorno JSON
      Result := LAppResource.ParseFind(LQuery);
      // Add Count Record no JSON Result
//      if LQuery.Count then
    end
    else
      raise Exception.Create('Class ' + LQuery.ResourceName + 'not found!');
  finally
    LAppResource.Free;
    LQuery.Free;
  end;
end;

function TServerMethods.insert(const AParams: TDWParams): String;
var
  LAppResource: TAppResourceBase;
  LParam: TJSONParam;
  LBody: TJSONParam;
begin
  LParam := nil;
  LParam := AParams.ItemsString['resource'];
  if LParam = nil then
    raise Exception.Create('resource parameter not passed');
  LAppResource := TAppResourceBase.Create(TRESTServerDWCore.GetConnection);
  try
    LBody := nil;
    LBody := AParams.ItemsString['json'];
    if LBody = nil then
      raise Exception.Create('body parameter not passed');
    Result := LAppResource.insert(LParam.AsString, LBody.AsString);
  finally
    LAppResource.Free;
  end;
end;

function TServerMethods.update(const AParams: TDWParams): String;
var
  LAppResource: TAppResourceBase;
  LParam: TJSONParam;
  LBody: TJSONParam;
begin
  LParam := nil;
  LParam := AParams.ItemsString['resource'];
  if LParam = nil then
    raise Exception.Create('resource parameter not passed');
  LAppResource := TAppResourceBase.Create(TRESTServerDWCore.GetConnection);
  try
    LBody := nil;
    LBody := AParams.ItemsString['json'];
    if LBody = nil then
      raise Exception.Create('body parameter not passed');
    Result := LAppResource.update(LParam.AsString, LBody.AsString);
  finally
    LAppResource.Free;
  end;
end;

function TServerMethods.delete(const AParams: TDWParams): String;
var
  LAppResource: TAppResourceBase;
  LQuery: TRESTQuery;
  LParam: TJSONParam;
begin
  LParam := nil;
  LParam := AParams.ItemsString['resource'];
  if LParam = nil then
    raise Exception.Create('resource parameter not passed');

  LQuery := TRESTQuery.Create;
  LAppResource := TAppResourceBase.Create(TRESTServerDWCore.GetConnection);
  try
    // Parse da Query passada na URI
    LQuery.ParseQuery(LParam.AsString);
    if LQuery.ResourceName <> '' then
    begin
      LParam := nil;
      LParam := AParams.ItemsString['$filter'];
      if LParam <> nil then
        LQuery.SetFilter(LParam.AsString);
      // Retorno JSON
      Result := LAppResource.ParseDelete(LQuery);
    end
    else
      raise Exception.Create('Class ' + LQuery.ResourceName + 'not found!');
  finally
    LAppResource.Free;
    LQuery.Free;
  end;
end;

function TServerMethods.GetParamValue(const AValue: String): String;
begin
  Result := AValue; // DecodeStrings(AValue);
end;

end.

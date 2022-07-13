{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Resources;

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  /// WiRL
  WiRL.Core.Engine,
  WiRL.Core.Application,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.http.URL,
  WiRL.Core.MessageBody.Default,
  WiRL.Core.Auth.Context,
  WiRL.http.Request,
  WiRL.http.Response,
  /// JSON
  JSON,
  /// ORMBr JSON
  ormbr.rest.json,
  ormbr.json.utils,
  /// ORMBr conexão database
  ormbr.types.database,
  ormbr.factory.firedac,
  ormbr.factory.interfaces,
  ormbr.dml.generator.firebird,
  /// ORMBr
  ormbr.container.objectset,
  ormbr.container.objectset.interfaces,
  /// ORMBr Models
  ormbr.model.master,
  ormbr.model.detail,
  ormbr.model.lookup,
  ormbr.model.client;

type
  [Path('/lookup')]
  TLookupResource = class
  private
    FConnection: IDBConnection;
    FLookup: IContainerObjectSet<Tlookup>;
  public
    constructor Create;
    destructor Destroy; override;

    [GET]
    [Produces(TMediaType.APPLICATION_JSON)]
    function select: TJSONArray; overload;
  end;

  [Path('/master')]
  TMasterResource = class
  private
    FConnection: IDBConnection;
    FMaster: IContainerObjectSet<Tmaster>;
  public
    constructor Create;
    destructor Destroy; override;

    [GET]
    [Produces(TMediaType.APPLICATION_JSON)]
    function select: TJSONArray;

    [GET, Path('/nextpacket/{APageSize}/{APageNext}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function nextpacket([PathParam] APageSize: Integer;
                        [PathParam] APageNext: Integer): TJSONArray;

    [GET, Path('/selectid/{AID}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function selectid([PathParam] AID: Integer): TJSONValue;

    [GET, Path('/selectwhere/{AWhere}/{AOrderBy}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function selectwhere([PathParam] AWhere: String;
                         [PathParam] AOrderBy: String): TJSONArray;

    [GET, Path('/nextpacketwhere/{AWhere}/{AOrderBy}/{APageSize}/{APageNext}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function nextpacketwhere([PathParam] AWhere: String;
                             [PathParam] AOrderBy: String;
                             [PathParam] APageSize: Integer;
                             [PathParam] APageNext: Integer): TJSONArray;

    [POST]
    [Produces(TMediaType.TEXT_PLAIN)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function insert([BodyParam] AValue: TJSONValue): String;

    [PUT]
    [Produces(TMediaType.TEXT_PLAIN)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function update([BodyParam] AValue: TJSONValue): String;

    [DELETE, Path('{AID}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function delete([PathParam] AID: Integer): String;
  end;

implementation

uses
  DateUtils,
  StrUtils,
  IOUtils,
  Server.Datamodule;

{ TMasterResource }

constructor TMasterResource.Create;
begin
  FConnection := TFactoryFireDAC.Create(ServerDataModule.FDConnection1, dnFirebird);
  FMaster := TContainerObjectSet<Tmaster>.Create(FConnection);
end;

destructor TMasterResource.Destroy;
begin

  inherited;
end;

function TMasterResource.select: TJSONArray;
var
  LMasterList: TObjectList<Tmaster>;
begin
  try
    LMasterList := FMaster.Find;
    /// <summary>
    /// Retorna o JSON
    /// </summary>
    Result := TORMBrJSONUtil.JSONObjectListToJSONArray<Tmaster>(LMasterList);
  finally
    if LMasterList <> nil then
      LMasterList.Free;
  end;
end;

function TMasterResource.selectid(AID: Integer): TJSONValue;
var
  LMaster: Tmaster;
begin
  try
    LMaster := FMaster.Find(AID);
    /// <summary>
    /// Retorna o JSON
    /// </summary>
    Result := TORMBrJSONUtil.JSONObjectToJSONValue(LMaster);
  finally
    if LMaster <> nil then
      LMaster.Free;
  end;
end;

function TMasterResource.selectwhere(AWhere, AOrderBy: String): TJSONArray;
var
  LMasterList: TObjectList<Tmaster>;
begin
  if AOrderBy = 'None' then
    AOrderBy := '';
  try
    LMasterList := FMaster.FindWhere(AWhere, AOrderBy);
    /// <summary>
    ///   Retorna o JSON
    /// </summary>
    Result := TORMBrJSONUtil.JSONObjectListToJSONArray<Tmaster>(LMasterList);
  finally
    if LMasterList <> nil then
      LMasterList.Free;
  end;
end;

function TMasterResource.insert(AValue: TJSONValue): String;
var
  LMaster: Tmaster;
begin
  try
    try
      LMaster := TORMBrJson.JsonToObject<Tmaster>(AValue.ToJSON);
      FMaster.Insert(LMaster);
      Result := '{"message":"registro inserido com sucesso!", "params":[{"master_id":' + IntToStr(LMaster.master_id) +'}]}';
    finally
      if LMaster <> nil then
        LMaster.Free;
    end;
  except
    on E: Exception do
    begin
      Result := E.Message;
    end;
  end;
end;

function TMasterResource.nextpacket(APageSize, APageNext: Integer): TJSONArray;
var
  LMasterList: TObjectList<Tmaster>;
begin
  try
    LMasterList := FMaster.NextPacket(APageSize, APageNext);
    /// <summary>
    /// Retorna o JSON
    /// </summary>
    Result := TORMBrJSONUtil.JSONObjectListToJSONArray<Tmaster>(LMasterList);
  finally
    if LMasterList <> nil then
      LMasterList.Free;
  end;
end;

function TMasterResource.nextpacketwhere(AWhere, AOrderBy: String; APageSize,
  APageNext: Integer): TJSONArray;
var
  LMasterList: TObjectList<Tmaster>;
begin
  if AOrderBy = 'None' then AOrderBy := '';
  try
    LMasterList := FMaster.NextPacket(AWhere, AOrderBy, APageSize, APageNext);
    /// <summary>
    /// Retorna o JSON
    /// </summary>
    Result := TORMBrJSONUtil.JSONObjectListToJSONArray<Tmaster>(LMasterList);
  finally
    if LMasterList <> nil then
      LMasterList.Free;
  end;
end;

function TMasterResource.update(AValue: TJSONValue): String;
var
  LMasterNew: Tmaster;
  LMasterOld: Tmaster;
begin
  try
    try
      LMasterNew := TORMBrJson.JsonToObject<Tmaster>(AValue.ToJSON);
      LMasterOld := FMaster.Find(LMasterNew.master_id);
      if LMasterOld <> nil then
      begin
        FMaster.Modify(LMasterOld);
        FMaster.Update(LMasterNew);
      end;
      Result := '{"message":"teste"}'
    finally
      if LMasterOld <> nil then
        LMasterOld.Free;
      if LMasterNew <> nil then
        LMasterNew.Free;
    end;
  except
    on E: Exception do
    begin
      Result := E.Message;
    end;
  end;
end;

function TMasterResource.delete(AID: Integer): String;
var
  LMaster: Tmaster;
begin
  try
    try
      LMaster := FMaster.Find(AID);
      FMaster.Delete(LMaster);
      Result := '{"message":"Dados excluídos do banco com sucesso!!!"}';
    finally
      if LMaster <> nil then
        LMaster.Free;
    end;
  except
    on E: Exception do
    begin
      Result := E.Message;
    end;
  end;
end;

{ TLookupResource }

constructor TLookupResource.Create;
begin
  FConnection := TFactoryFireDAC.Create(ServerDataModule.FDConnection1, dnSQLite);
  FLookup := TContainerObjectSet<Tlookup>.Create(FConnection);
end;

destructor TLookupResource.Destroy;
begin

  inherited;
end;

function TLookupResource.select: TJSONArray;
var
  LMasterList: TObjectList<Tlookup>;
begin
  try
    LMasterList := FLookup.Find;
    /// <summary>
    /// Retorna o JSON
    /// </summary>
    Result := TORMBrJSONUtil.JSONObjectListToJSONArray<Tlookup>(LMasterList);
  finally
    if LMasterList <> nil then
      LMasterList.Free;
  end;
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TLookupResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TMasterResource>;

end.

(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Resources;

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  /// MARS
  MARS.Core.Exceptions,
  MARS.Core.Registry,
  MARS.Core.Attributes,
  MARS.Core.MediaType,
  MARS.Core.JSON,
  MARS.Core.MessageBodyWriters,
  MARS.Core.MessageBodyReaders,
  MARS.Core.URL,
  /// JSON
  JSON,
  /// ORMBr JSON
  ormbr.rest.json,
  ormbr.json.utils,
  /// ORMBr conexão database
  ormbr.types.database,
  ormbr.factory.firedac,
  ormbr.factory.interfaces,
  /// ORMBr
  ormbr.container.objectset,
  ormbr.container.objectset.interfaces,
  /// ORMBr Models
  ormbr.model.master,
  ormbr.model.detail,
  ormbr.model.lookup;

type
  [Path('/lookup'), Produces(TMediaType.TEXT_PLAIN)]
  TLookupResource = class
  private
    FConnection: IDBConnection;
    FLookup: IContainerObjectSet<Tlookup>;
  public
    constructor Create;
    destructor Destroy; override;

    [GET]
    [Produces(TMediaType.APPLICATION_JSON)]
    function select: TJSONArray;
  end;

  [Path('/master'), Produces(TMediaType.TEXT_PLAIN)]
  TMasterResource = class
  private
    FConnection: IDBConnection;
    FMaster: IContainerObjectSet<Tmaster>;
    [Context] URL: TMARSURL;
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
    [Produces(TMediaType.APPLICATION_JSON)]
    function insert([BodyParam] AValue: TJSONValue): TJSONValue;

    [PUT]
    [Produces(TMediaType.APPLICATION_JSON)]
    function update([BodyParam] AValue: TJSONValue): TJSONString;

    [DELETE, Path('{AID}')]
    [Produces(TMediaType.TEXT_PLAIN)]
    [Produces(TMediaType.APPLICATION_JSON)]
    function delete([PathParam] AID: Integer): TJSONString;
  end;

implementation

uses
  DateUtils,
  StrUtils,
  IOUtils,
  Server.Data.Main;

{ TMasterResource }

constructor TMasterResource.Create;
begin
  FConnection := TFactoryFireDAC.Create(ServerDataModule.FDConnection1, dnSQLite);
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
  if AOrderBy = 'None' then AOrderBy := '';
  try
    LMasterList := FMaster.FindWhere(AWhere, AOrderBy);
    /// <summary>
    /// Retorna o JSON
    /// </summary>
    Result := TORMBrJSONUtil.JSONObjectListToJSONArray<Tmaster>(LMasterList);
  finally
    if LMasterList <> nil then
      LMasterList.Free;
  end;
end;

function TMasterResource.insert(AValue: TJSONValue): TJSONValue;
var
  LMaster: Tmaster;
begin
  try
    try
      LMaster := TORMBrJson.JsonToObject<Tmaster>(AValue.ToJSON);
      if LMaster <> nil then
        FMaster.Insert(LMaster);
      Result := TORMBrJSONUtil
                  .JSONStringToJSONValue('{"message":"registro inserido com sucesso!", "params":[{"master_id":' + IntToStr(LMaster.master_id) +'}]}');
    finally
      if LMaster <> nil then
        LMaster.Free;
    end;
  except
    on E: Exception do
    begin
      Result := TJSONString.Create(E.Message);
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
  if AOrderBy = 'None' then
    AOrderBy := '';
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

function TMasterResource.update(AValue: TJSONValue): TJSONString;
var
  LMasterNew: Tmaster;
  LMasterOld: Tmaster;
  LFor: Integer;
begin
  try
    try
      LMasterNew := TORMBrJson.JsonToObject<Tmaster>(AValue.ToJSON);
      if LMasterNew <> nil then
      begin
        LMasterOld := FMaster.Find(LMasterNew.master_id);
        if LMasterOld <> nil then
        begin
          FMaster.Modify(LMasterOld);
          FMaster.Update(LMasterNew);
        end;
      end;
      Result := TJSONString.Create('Dados alterado no banco com sucesso!!!');
    finally
      if LMasterOld <> nil then
        LMasterOld.Free;
      if LMasterNew <> nil then
        LMasterNew.Free;
    end;
  except
    on E: Exception do
    begin
      Result := TJSONString.Create(E.Message);
    end;
  end;
end;

function TMasterResource.delete(AID: Integer): TJSONString;
var
  LMaster: Tmaster;
begin
  try
    try
      LMaster := FMaster.Find(AID);
      FMaster.Delete(LMaster);
      Result := TJSONString.Create('Dados excluídos do banco com sucesso!!!');
    finally
      if LMaster <> nil then
        LMaster.Free;
    end;
  except
    on E: Exception do
    begin
      Result := TJSONString.Create(E.Message);
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
  TMARSResourceRegistry.Instance.RegisterResource<TLookupResource>;
  TMARSResourceRegistry.Instance.RegisterResource<TMasterResource>;

end.

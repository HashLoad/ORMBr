unit Server.Resources;

interface

uses
  Windows,
  SysUtils,
  StrUtils,
  Classes,
  JSON,
  SysTypes,
  Generics.Collections,
  uDWConsts,
  uDWConstsData,
  /// ORMBr JSON
  ormbr.rest.json,
  ormbr.json.utils,
  /// ORMBr conexão database
  ormbr.factory.firedac,
  ormbr.factory.interfaces,
  /// ORMBr
  ormbr.container.objectset,
  ormbr.container.objectset.interfaces,
  /// ORMBr Models
  ormbr.model.master,
  ormbr.model.detail,
  ormbr.model.lookup;
  /// ORMBr banco usado

type
  TMasterResource = class
  private
    FConnection: IDBConnection;
    FMaster: IContainerObjectSet<Tmaster>;
    FLookup: IContainerObjectSet<Tlookup>;
  public
   { Public declarations }
    constructor Create;
    destructor Destroy; override;
    function select: TJSONArray;
    function nextpacket(APageSize, APageNext: Integer): TJSONArray;
    function selectid(AID: Integer): String;
    function selectwhere(AWhere, AOrderBy: String): TJSONArray;
    function nextpacketwhere(AWhere, AOrderBy: String; APageSize, APageNext: Integer): TJSONArray;
    function insert(AValue: TJSONValue): String;
    function update(AValue: TJSONValue): String;
    function delete(AID: Integer): String;
    function selectlookup: TJSONArray;
  end;

implementation

uses
  Server.Datamodule;

constructor TMasterResource.Create;
begin
  FConnection := TFactoryFireDAC.Create(ServerDataModule.FDConnection1, dnSQLite);
  FMaster := TContainerObjectSet<Tmaster>.Create(FConnection);
  FLookup := TContainerObjectSet<Tlookup>.Create(FConnection);
end;

destructor TMasterResource.Destroy;
begin

  inherited Destroy;
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

function TMasterResource.selectid(AID: Integer): String;
var
  LMaster: Tmaster;
begin
  try
    LMaster := FMaster.Find(AID);
    /// <summary>
    /// Retorna o JSON
    /// </summary>
    Result := TORMBrJson.ObjectToJsonString(LMaster);
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
    /// Retorna o JSON
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
      if LMaster <> nil then
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
  if AOrderBy = 'None' then
    AOrderBy := '';
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

function TMasterResource.update(AValue: TJSONValue): String;
var
  LMasterNew: Tmaster;
  LMasterOld: Tmaster;
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
      Result := 'Dados alterado no banco com sucesso!!!';
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
      Result := 'Dados excluídos do banco com sucesso!!!';
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

function TMasterResource.selectlookup: TJSONArray;
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

end.





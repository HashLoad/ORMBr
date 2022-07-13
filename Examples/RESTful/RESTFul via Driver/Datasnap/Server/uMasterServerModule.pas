unit uMasterServerModule;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Json,
  DataSnap.DSProviderDataModuleAdapter,
  Datasnap.DSServer,
  Datasnap.DSAuth,
  Datasnap.DSSession,
  System.Generics.Collections,
  /// ORMBr Conexão database
  ormbr.factory.firedac,
  ormbr.factory.interfaces,
  /// ORMBr
  ormbr.container.objectset,
  ormbr.container.objectset.interfaces,
  /// Classes modelos
  ormbr.model.master,
  ormbr.model.detail,
  /// ORMBr Banco usado
  ormbr.dml.generator.sqlite;

type
  Tapimaster = class(TDSServerModule)
    procedure DSServerModuleCreate(Sender: TObject);
  private
    { Private declarations }
    FConnection: IDBConnection;
    FMaster: IContainerObjectSet<Tmaster>;
  public
    { Public declarations }
    function master: TJSONArray;
    function selectid(AID: Integer): TJSONValue;
    function selectwhere(AWhere: String; AOrderBy: String = ''): TJSONArray;
    function nextpacket(APageSize, APageNext: Integer): TJSONArray;
    function nextpacketwhere(AWhere,
                             AOrderBy: String;
                             APageSize,
                             APageNext: Integer): TJSONArray;
    function acceptmaster(AValue: TJSONValue): TJSONValue;
    function updatemaster(AValue: TJSONValue): TJSONValue;
    function cancelmaster(AID: Integer): TJSONValue;
  end;

implementation

uses
  /// ORMBr JSON
  ormbr.rest.json,
  ormbr.json.utils,
  /// Datamodule
  uDataModuleServer;

{$R *.dfm}

{ TServerMethods1 }

function Tapimaster.acceptmaster(AValue: TJSONValue): TJSONValue;
var
  LMaster: Tmaster;
begin
  try
    LMaster := TORMBrJson.JsonToObject<Tmaster>(AValue.ToJSON);
    try
      if LMaster <> nil then
        FMaster.Insert(LMaster);

      Result := TORMBrJSONUtil
                  .JSONStringToJSONValue('{"message":"registro inserido com sucesso!", "params":[{"master_id":' + IntToStr(LMaster.master_id) +'}]}');
    finally
      LMaster.Free;
    end;
  except
    on E: Exception do
    begin
      Result := TORMBrJSONUtil.JSONStringToJSONValue('{"result":"' + E.Message + '"}');
    end;
  end;
end;

function Tapimaster.cancelmaster(AID: Integer): TJSONValue;
var
  LMaster: Tmaster;
begin
  try
    LMaster := FMaster.Find(AID);
    try
      FMaster.Delete(LMaster);
      Result := TORMBrJSONUtil
                  .JSONStringToJSONValue('{"message":"Dados excluídos do banco com sucesso!!!"}');
    finally
      if LMaster <> nil then
        LMaster.Free;
    end;
  except
    on E: Exception do
    begin
      Result := TORMBrJSONUtil.JSONStringToJSONValue('{"result":"' + E.Message + '"}');
    end;
  end;
end;

procedure Tapimaster.DSServerModuleCreate(Sender: TObject);
begin
  FConnection := TFactoryFireDAC.Create(DataModuleServer.FDConnection1, dnSQLite);
  FMaster := TContainerObjectSet<Tmaster>.Create(FConnection);
end;

function Tapimaster.master: TJSONArray;
var
  LMasterList: TObjectList<Tmaster>;
begin
  try
    LMasterList := FMaster.Find;
    /// <summary>
    /// Retorna  JSON
    /// </summary>
    Result := TORMBrJSONUtil.JSONObjectListToJSONArray<Tmaster>(LMasterList);
  finally
    LMasterList.Free;
  end;
end;

function Tapimaster.nextpacket(APageSize, APageNext: Integer): TJSONArray;
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

function Tapimaster.nextpacketwhere(AWhere, AOrderBy: String; APageSize,
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

function Tapimaster.selectwhere(AWhere, AOrderBy: String): TJSONArray;
var
  LMasterList: TObjectList<Tmaster>;
begin
  try
    LMasterList := FMaster.FindWhere(AWhere, AOrderBy);
    /// <summary>
    /// Retorna  JSON
    /// </summary>
    Result := TORMBrJSONUtil.JSONObjectListToJSONArray<Tmaster>(LMasterList);
  finally
    LMasterList.Free;
  end;
end;

function Tapimaster.updatemaster(AValue: TJSONValue): TJSONValue;
var
  LMasterNew: Tmaster;
  LMasterOld: Tmaster;
begin
  try
    LMasterNew := TORMBrJson.JsonToObject<Tmaster>(AValue.ToJSON);
    try
      if LMasterNew <> nil then
      begin
        LMasterOld := FMaster.Find(LMasterNew.master_id);
        FMaster.Modify(LMasterOld);
        FMaster.Update(LMasterNew);
      end;
      Result := TORMBrJSONUtil
                  .JSONStringToJSONValue('{"message":"Dados alterado no banco com sucesso!!!"}');
    finally
      if LMasterOld <> nil then
        LMasterOld.Free;
      if LMasterNew <> nil then
        LMasterNew.Free;
    end;
  except
    on E: Exception do
    begin
      Result := TORMBrJSONUtil.JSONStringToJSONValue('{"result":"' + E.Message + '"}');
    end;
  end;
end;

function Tapimaster.selectid(AID: Integer): TJSONValue;
var
  LMaster: Tmaster;
begin
  try
    LMaster := FMaster.Find(AID);
    /// <summary>
    /// Retorna  JSON
    /// </summary>
    Result := TORMBrJSONUtil.JSONObjectToJSONValue(LMaster);
  finally
    LMaster.Free;
  end;
end;

end.


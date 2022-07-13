unit Server.Resource;

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  /// Delphi MVC
  MVCFramework.Commons,
  MVCFramework,

  /// JSON
  JSON,
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
  ormbr.model.client,
  ormbr.model.master,
  ormbr.model.detail,
  ormbr.model.lookup;

type
  [MVCPath('/lookup')]
  TLookupController = class(TMVCController)
  private
    FConnection: IDBConnection;
    FLookup: IContainerObjectSet<Tlookup>;
  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionName: string;
      var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext; const AActionName: string); override;
  public
    constructor Create;
    destructor Destroy; override;

    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure select;
  end;

  [MVCPath('/master')]
  TMasterController = class(TMVCController)
  private
    FConnection: IDBConnection;
    FMaster: IContainerObjectSet<Tmaster>;
  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionName: string;
      var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext; const AActionName: string); override;
  public
    [MVCPath]
    [MVCHTTPMethod([httpGET])]
    procedure select;

    [MVCPath('/selectid/($AID)')]
    [MVCHTTPMethod([httpGET])]
    procedure selectid(AID: Integer);

    [MVCPath('/selectwhere/($AWhere)/($AOrderBy)')]
    [MVCHTTPMethod([httpGET])]
    procedure selectwhere(AWhere: String;
                          AOrderBy: String);

    [MVCPath('/nextpacket/($APageSize)/($APageNext)')]
    [MVCHTTPMethod([httpGET])]
    procedure nextpacket(APageSize,
                         APageNext: Integer);

    [MVCPath('/nextpacketwhere/($AWhere)/($AOrderBy)/($APageSize)/($APageNext)')]
    [MVCHTTPMethod([httpGET])]
    procedure nextpacketwhere(AWhere,
                              AOrderBy: String;
                              APageSize,
                              APageNext: Integer);

    [MVCPath('/($AValue)')]
    [MVCHTTPMethod([httpPOST])]
    procedure insert(AValue: String);

    [MVCPath('/($AValue)')]
    [MVCHTTPMethod([httpPUT])]
    procedure update(AValue: String);

    [MVCPath('/($AID)')]
    [MVCHTTPMethod([httpDELETE])]
    procedure delete([PathParam] AID: Integer);
  end;

implementation

uses
  Server.Data.Module, IdURI;

procedure TMasterController.delete(AID: Integer);
var
  LMaster: Tmaster;
begin
  try
    try
      LMaster := FMaster.Find(AID);
      FMaster.Delete(LMaster);
      Render(TJSONString.Create('Dados excluídos do banco com sucesso!!!').ToJSON);
    finally
      if LMaster <> nil then
        LMaster.Free;
    end;
  except
    on E: Exception do
    begin
      Render(E.Message);
    end;
  end;
end;

procedure TMasterController.insert(AValue: String);
var
  LMaster: Tmaster;
begin
  try
    LMaster := TORMBrJson.JsonToObject<Tmaster>(AValue);
    try
      if LMaster <> nil then
        FMaster.Insert(LMaster);
      Render('{"message":"registro inserido com sucesso!", "params":[{"master_id":' + IntToStr(LMaster.master_id) +'}]}');
    finally
      if LMaster <> nil then
        LMaster.Free;
    end;
  except
    on E: Exception do
    begin
      Render(E.Message);
    end;
  end;
end;

procedure TMasterController.nextpacket(APageSize, APageNext: Integer);
var
  LMasterList: TObjectList<Tmaster>;
begin
  try
    LMasterList := FMaster.NextPacket(APageSize, APageNext);
    /// <summary>
    /// Retorna o JSON
    /// </summary>
    Render(TORMBrJson.ObjectListToJsonString<Tmaster>(LMasterList));
  finally
    if LMasterList <> nil then
      LMasterList.Free;
  end;
end;

procedure TMasterController.nextpacketwhere(AWhere, AOrderBy: String; APageSize,
  APageNext: Integer);
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
    Render(TORMBrJson.ObjectListToJsonString<Tmaster>(LMasterList));
  finally
    if LMasterList <> nil then
      LMasterList.Free;
  end;
end;

procedure TMasterController.select;
var
  LMasterList: TObjectList<Tmaster>;
begin
  try
    LMasterList := FMaster.Find;
    /// <summary> Retorna o JSON </summary>
    Render(TORMBrJson.ObjectListToJsonString<Tmaster>(LMasterList));
  finally
    if LMasterList <> nil then
      LMasterList.Free;
  end;
end;

procedure TMasterController.selectid(AID: Integer);
var
  LMaster: Tmaster;
begin
  try
    LMaster := FMaster.Find(AID);
    /// <summary> Retorna o JSON </summary>
    Render(TORMBrJson.ObjectToJsonString(LMaster));
  finally
    if LMaster <> nil then
      LMaster.Free;
  end;
end;

procedure TMasterController.selectwhere(AWhere, AOrderBy: String);
var
  LMasterList: TObjectList<Tmaster>;
begin
  if AOrderBy = 'None' then
    AOrderBy := '';
  try
    LMasterList := FMaster.FindWhere(AWhere, AOrderBy);
    /// <summary> Retorna o JSON </summary>
    Render(TORMBrJson.ObjectListToJsonString<Tmaster>(LMasterList));
  finally
    if LMasterList <> nil then
      LMasterList.Free;
  end;
end;

procedure TMasterController.update(AValue: String);
var
  LMasterNew: Tmaster;
  LMasterOld: Tmaster;
  LFor: Integer;
begin
  try
    LMasterNew := TORMBrJson.JsonToObject<Tmaster>(AValue);
    try
      if LMasterNew <> nil then
      begin
        LMasterOld := FMaster.Find(LMasterNew.master_id);
        try
          if LMasterOld <> nil then
          begin
            FMaster.Modify(LMasterOld);
            FMaster.Update(LMasterNew);
          end;
        finally
          if LMasterOld <> nil then
            LMasterOld.Free;
        end;
      end;
      Render(TJSONString.Create('Dados alterado no banco com sucesso!!!').ToJSON);
    finally
      if LMasterNew <> nil then
        LMasterNew.Free;
    end;
  except
    on E: Exception do
    begin
      Render(E.Message);
    end;
  end;
end;

procedure TMasterController.OnAfterAction(Context: TWebContext;
  const AActionName: string);
begin
  { Executed after each action }
  inherited;
end;

procedure TMasterController.OnBeforeAction(Context: TWebContext;
  const AActionName: string; var Handled: Boolean);
begin
  FConnection := TFactoryFireDAC.Create(ServerDataModule.FDConnection1, dnSQLite);
  Fmaster := TContainerObjectSet<Tmaster>.Create(FConnection);
  inherited;
end;

{ TLookupController }

constructor TLookupController.Create;
begin
  inherited;
end;

destructor TLookupController.Destroy;
begin

  inherited;
end;

procedure TLookupController.OnAfterAction(Context: TWebContext;
  const AActionName: string);
begin
  inherited;

end;

procedure TLookupController.OnBeforeAction(Context: TWebContext;
  const AActionName: string; var Handled: Boolean);
begin
  FConnection := TFactoryFireDAC.Create(ServerDataModule.FDConnection1, dnSQLite);
  FLookup := TContainerObjectSet<Tlookup>.Create(FConnection);
  inherited;
end;

procedure TLookupController.select;
var
  LMasterList: TObjectList<Tlookup>;
begin
  try
    LMasterList := FLookup.Find;
    /// <summary> Retorna o JSON </summary>
    Render(TORMBrJson.ObjectListToJsonString<Tlookup>(LMasterList));
  finally
    if LMasterList <> nil then
      LMasterList.Free;
  end;
end;

end.

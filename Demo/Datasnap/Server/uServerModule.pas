unit uServerModule;

interface

uses
  System.SysUtils, System.Classes, System.Json,
  DataSnap.DSProviderDataModuleAdapter,
  Datasnap.DSServer,
  Datasnap.DSAuth,
  Datasnap.DSSession,
  System.Generics.Collections,
  /// ORMBr JSON e DataSnap
  ormbr.rest.json,
  ormbr.json.utils,
  /// ORMBr Conexão database
  ormbr.factory.firedac,
  ormbr.factory.interfaces,
  ormbr.types.database,
  /// ORMBr
  ormbr.container.objectset,
  ormbr.container.objectset.interfaces,
  ormbr.session.dataset,
  ormbr.model.master,
  ormbr.model.detail,
  ormbr.model.lookup,
  ormbr.dml.generator.sqlite,

  uServerContainer,
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.VCLUI.Wait, Data.DB,
  FireDAC.Comp.Client;

type
  TORMBr = class(TDSServerModule)
    FDConnection1: TFDConnection;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
    FSession: TDSSession;
    FConnectionKey: string;
    FMasterKey: string;
    FConnection: IDBConnection;
    FContainerMaster: IContainerObjectSet<Tmaster>;
    FContainerLookup: IContainerObjectSet<Tlookup>;
    procedure AddKeys;
    procedure DeleteKeys;
    procedure GeneratorKeys;
    procedure RecoversKeys;
    procedure ControleDeSessao;
  public
    { Public declarations }
    function lookup(AID: Integer = 0): TJSONArray;
    function master(AID: Integer = 0): TJSONArray;
    function masterWhere(AWhere: String; AOrderBy: String = ''): TJSONArray;
    function acceptmaster(AValue: TJSONArray): TJSONString;
    function updatemaster(AValue: TJSONArray): TJSONString;
    function cancelmaster(AID: Integer): TJSONString;
  end;

implementation

uses
  uFormServer;

{$R *.dfm}

{ TServerMethods1 }

function TORMBr.acceptmaster(AValue: TJSONArray): TJSONString;
var
  LMasterList: TObjectList<Tmaster>;
  LFor: Integer;
begin
  /// <summary>
  /// Controle se Sessão
  /// </summary>
//  ControleDeSessao;

  try
    LMasterList := TORMBrJson.JsonToObjectList<Tmaster>(AValue.ToJSON);
    try
      for LFor := 0 to LMasterList.Count -1 do
        FContainerMaster.Insert(LMasterList.Items[LFor]);
      Result := TJSONString.Create('Dados inserido no banco com sucesso!!!');
    finally
      LMasterList.Clear;
      LMasterList.Free;
    end;
  except
    Result := TJSONString.Create('Houve um erro ao tentar inserir os dados no banco!!!');
  end;
end;

procedure TORMBr.AddKeys;
begin
  GeneratorKeys;
  TServerContainer1.GetDictionary.Add(FConnectionKey, TFactoryFireDAC.Create(FDConnection1, dnSQLite));

  FConnection := TServerContainer1.GetDictionary.Items[FConnectionKey] as TFactoryFireDAC;
  TServerContainer1.GetDictionary.Add(FMasterKey, TContainerObjectSet<Tmaster>.Create(FConnection));

  FContainerLookup := TContainerObjectSet<Tlookup>.Create(FConnection);
end;

function TORMBr.cancelmaster(AID: Integer): TJSONString;
var
  LMaster: Tmaster;
begin
  /// <summary>
  /// Controle se Sessão
  /// </summary>
//  ControleDeSessao;

  try
    LMaster := FContainerMaster.Find(AID);
    FContainerMaster.Delete(LMaster);
    Result := TJSONString.Create('Dados excluídos do banco com sucesso!!!');
  except
    Result := TJSONString.Create('Houve um erro ao tentar excluir os dados no banco!!!');
  end;
end;

procedure TORMBr.ControleDeSessao;
begin
  DeleteKeys;
  AddKeys;
  RecoversKeys;
end;

procedure TORMBr.DataModuleCreate(Sender: TObject);
begin
  /// <summary>
  /// Controle se Sessão
  /// </summary>
  ControleDeSessao;
end;

procedure TORMBr.DeleteKeys;
begin
  GeneratorKeys;

  if TServerContainer1.GetDictionary.ContainsKey(FConnectionKey) then
    TServerContainer1.GetDictionary.Remove(FConnectionKey);
  if TServerContainer1.GetDictionary.ContainsKey(FMasterKey) then
    TServerContainer1.GetDictionary.Remove(FMasterKey);
end;

procedure TORMBr.GeneratorKeys;
begin
  FSession := TDSSessionManager.GetThreadSession;
  FConnectionKey := 'Connection_' + IntToStr(FSession.Id);
  FMasterKey := 'Master_' + IntToStr(FSession.Id);
end;

function TORMBr.lookup(AID: Integer): TJSONArray;
var
  LLookupList: TObjectList<Tlookup>;
begin
  /// <summary>
  /// Controle se Sessão
  /// </summary>
//  ControleDeSessao;

  LLookupList := TObjectList<Tlookup>.Create;
  try
    if AID = 0 then
      LLookupList := FContainerLookup.Find
    else
      LLookupList := FContainerLookup.FindWhere('lookup_id = ' + IntToStr(AID));
    /// <summary>
    /// Retorna o JSON
    /// </summary>
    Result := TORMBrJSONUtil.JSONObjectListToJSONArray<Tlookup>(LLookupList);
  finally
    LLookupList.Free;
  end;
end;

function TORMBr.masterWhere(AWhere, AOrderBy: String): TJSONArray;
var
  LMasterList: TObjectList<Tmaster>;
begin
  /// <summary>
  /// Controle se Sessão
  /// </summary>
//  ControleDeSessao;

  LMasterList := TObjectList<Tmaster>.Create;
  try
    LMasterList := FContainerMaster.FindWhere(AWhere, AOrderBy);
    /// <summary>
    /// Retorna o JSON
    /// </summary>
    Result := TORMBrJSONUtil.JSONObjectListToJSONArray<Tmaster>(LMasterList);
  finally
    LMasterList.Free;
  end;
end;

procedure TORMBr.RecoversKeys;
begin
  GeneratorKeys;
  FConnection := TServerContainer1.GetDictionary.Items[FConnectionKey] as TFactoryFireDAC;
  FContainerMaster := TServerContainer1.GetDictionary.Items[FMasterKey] as TContainerObjectSet<Tmaster>;
end;

function TORMBr.updatemaster(AValue: TJSONArray): TJSONString;
var
  LMasterList: TObjectList<Tmaster>;
  LMasterUpdate: Tmaster;
  LFor: Integer;
begin
  /// <summary>
  /// Controle se Sessão
  /// </summary>
//  ControleDeSessao;

  try
    LMasterList := TORMBrJson.JsonToObjectList<Tmaster>(AValue.ToJSON);
    try
      for LFor := 0 to LMasterList.Count -1 do
      begin
        LMasterUpdate := FContainerMaster.Find(LMasterList.Items[LFor].master_id);
        FContainerMaster.Modify(LMasterUpdate);
        FContainerMaster.Update(LMasterList.Items[LFor]);
      end;
      Result := TJSONString.Create('Dados alterado no banco com sucesso!!!');
    finally
      LMasterList.Clear;
      LMasterList.Free;
    end;
  except
    Result := TJSONString.Create('Houve um erro ao tentar alterar os dados no banco!!!');
  end;
end;

function TORMBr.master(AID: Integer): TJSONArray;
var
  LMasterList: TObjectList<Tmaster>;
begin
  /// <summary>
  /// Controle se Sessão
  /// </summary>
//  ControleDeSessao;

  LMasterList := TObjectList<Tmaster>.Create;
  try
    if AID = 0 then
      LMasterList := FContainerMaster.Find
    else
      LMasterList := FContainerMaster.FindWhere('master_id = ' + IntToStr(AID));
    /// <summary>
    /// Retorna o JSON
    /// </summary>
    Result := TORMBrJSONUtil.JSONObjectListToJSONArray<Tmaster>(LMasterList);
  finally
    LMasterList.Free;
  end;
end;

end.


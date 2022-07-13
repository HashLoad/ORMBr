unit uLookupServerModule;

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
  /// ORMBr
  ormbr.container.objectset,
  ormbr.container.objectset.interfaces,
  ormbr.session.dataset,
  ormbr.model.lookup,
  ormbr.dml.generator.sqlite;

type
  Tapilookup = class(TDSServerModule)
    procedure DSServerModuleCreate(Sender: TObject);
  private
    { Private declarations }
    FConnection: IDBConnection;
    FLookup: IContainerObjectSet<Tlookup>;
  public
    { Public declarations }
    function lookup: TJSONArray;
    function selectid(AID: Integer): TJSONValue;
    function selectwhere(AWhere: String; AOrderBy: String = ''): TJSONArray;
    function api(AResource: String): TJSONString;
    function acceptapi(AResource: String): TJSONString;
    /// <summary>
    /// "Suffix" é o nome definido na propriedade Post_Put_Delete_Suffix do
    /// componentes TRestDataSnapConnection
    /// </summary>
    function acceptlookup(AValue: TJSONArray): TJSONString;
    function updatelookup(AValue: TJSONArray): TJSONString;
    function cancellookup(AID: Integer): TJSONString;
  end;

implementation

uses
  uDataModuleServer;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

function Tapilookup.acceptlookup(AValue: TJSONArray): TJSONString;
begin

end;

function Tapilookup.cancellookup(AID: Integer): TJSONString;
begin

end;

procedure Tapilookup.DSServerModuleCreate(Sender: TObject);
begin
  FConnection := TFactoryFireDAC.Create(DataModuleServer.FDConnection1, dnSQLite);
  FLookup := TContainerObjectSet<Tlookup>.Create(FConnection);
end;

function Tapilookup.lookup: TJSONArray;
var
  LLookupList: TObjectList<Tlookup>;
begin
  try
    LLookupList := FLookup.Find;
    /// <summary>
    /// Retorna o JSON
    /// </summary>
    Result := TORMBrJSONUtil.JSONObjectListToJSONArray<Tlookup>(LLookupList);
  finally
    LLookupList.Free;
  end;
end;

function Tapilookup.selectid(AID: Integer): TJSONValue;
begin

end;

function Tapilookup.selectwhere(AWhere, AOrderBy: String): TJSONArray;
begin

end;

function Tapilookup.updatelookup(AValue: TJSONArray): TJSONString;
begin

end;

function Tapilookup.api(AResource: String): TJSONString;
begin
  Result := TJSONString.Create('{"GET":"' + AResource + '"}');
end;

function Tapilookup.acceptapi(AResource: String): TJSONString;
begin
  Result := TJSONString.Create('{"POST":"' + AResource + '"}');
end;

end.

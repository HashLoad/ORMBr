unit repository.master;

interface

uses
  DB,
  SysUtils,
  provider.ormbr,
  provider.datamodule,
  // ORMBr Modelos
  ormbr.model.master,
  ormbr.model.detail,
  ormbr.model.lookup,
  ormbr.model.client;

type
  TRepositoryMaster = class
  private
    FProvider: TProviderORMBr<Tmaster>;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function Execute(const AURL: String; const ARequestMethod: TRESTRequestMethodType;
      const AParamsProc: TProc): String;
    function Master: TDataSet;
    function Detail: TDataSet;
    function Client: TDataSet;
    function Lookup: TDataSet;
    function ProviderDM: TProviderDM;
    procedure Open;
    procedure OpenWhere(const AWhere: String);
    procedure ApplyUpdates;
    procedure MonitorShow;
  end;

implementation

{ TRepositoryClient }

function TRepositoryMaster.Client: TDataSet;
begin
  Result := FProvider.DataSet<TClient>;
end;

constructor TRepositoryMaster.Create;
begin
  FProvider := TProviderORMBr<Tmaster>.Create;
  FProvider.AddAdapter(FProvider.ProviderDM.FDMaster, 3)
           .AddChild<Tdetail>(FProvider.ProviderDM.FDDetail)
           .AddChild<Tclient>(FProvider.ProviderDM.FDClient)
           .AddAdapter<Tlookup>(FProvider.ProviderDM.FDLookup)
end;

destructor TRepositoryMaster.Destroy;
begin
  FProvider.Free;
  inherited;
end;

function TRepositoryMaster.Detail: TDataSet;
begin
  Result := FProvider.DataSet<TDetail>;
end;

function TRepositoryMaster.Execute(const AURL: String; const ARequestMethod: TRESTRequestMethodType;
      const AParamsProc: TProc): String;
begin
  Result := FProvider.Execute(AURL, ARequestMethod, AParamsProc);
end;

function TRepositoryMaster.Lookup: TDataSet;
begin
  Result := FProvider.DataSet<TLookup>;
end;

function TRepositoryMaster.Master: TDataSet;
begin
  Result := FProvider.DataSet<TMaster>;
end;

procedure TRepositoryMaster.MonitorShow;
begin
  FProvider.MonitorShow;
end;

procedure TRepositoryMaster.Open;
begin
  FProvider.Open;
end;

procedure TRepositoryMaster.OpenWhere(const AWhere: String);
begin
  FProvider.OpenWhere(AWhere);
end;

function TRepositoryMaster.ProviderDM: TProviderDM;
begin
  Result := FProvider.ProviderDM;
end;

procedure TRepositoryMaster.ApplyUpdates;
begin
  FProvider.ApplyUpdates(0);
end;

end.

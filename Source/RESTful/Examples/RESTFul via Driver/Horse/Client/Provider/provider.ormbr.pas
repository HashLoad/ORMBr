unit provider.ormbr;

interface

uses
  DB,
  SysUtils,
  provider.datamodule,
  // ORMBr Manager
  ormbr.client.methods,
  ormbr.manager.dataset,
  ormbr.client.methods;

type
  TRESTRequestMethodType = ormbr.client.methods.TRESTRequestMethodType;
  TProviderORMBr<T: class, constructor> = class
  private
    FManager: TManagerDataSet;
    FProviderDM: TProviderDM;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Open;
    procedure OpenWhere(const AWhere: String);
    procedure ApplyUpdates(MaxErros: Integer);
    procedure MonitorShow;
    function Execute(const AURL: String; const ARequestMethod: TRESTRequestMethodType;
      const AParamsProc: TProc): String;
    function ProviderDM: TProviderDM;
    function DataSet<D: class, constructor>: TDataSet;
    function AddAdapter(const ADataSet: TDataSet; const APageSize: Integer = -1): TProviderORMBr<T>; overload;
    function AddAdapter<A: class, constructor>(const ADataSet: TDataSet; const APageSize: Integer = -1): TProviderORMBr<T>; overload;
    function AddChild<C: class, constructor>(const ADataSet: TDataSet): TProviderORMBr<T>;
  end;

implementation

uses
  ormbr.form.monitor;

{ TProviderClient }

function TProviderORMBr<T>.AddAdapter(const ADataSet: TDataSet;
  const APageSize: Integer): TProviderORMBr<T>;
begin
  Result := Self;
  FManager.AddAdapter<T>(ADataSet, APageSize);
end;

function TProviderORMBr<T>.AddChild<C>(const ADataSet: TDataSet): TProviderORMBr<T>;
begin
  Result := Self;
  FManager.AddAdapter<C, T>(ADataSet);
end;

function TProviderORMBr<T>.AddAdapter<A>(const ADataSet: TDataSet;
  const APageSize: Integer): TProviderORMBr<T>;
begin
  Result := Self;
  FManager.AddAdapter<A>(ADataSet, APageSize);
end;

procedure TProviderORMBr<T>.ApplyUpdates(MaxErros: Integer);
begin
  FManager.ApplyUpdates<T>(0);
end;

constructor TProviderORMBr<T>.Create;
begin
  FProviderDM := TProviderDM.Create(nil);
  FProviderDM.RESTClientHorse1.AsConnection.SetCommandMonitor(TCommandMonitor.GetInstance);
  // Manager
  FManager := TManagerDataSet.Create(FProviderDM.RESTClientHorse1.AsConnection);
end;

destructor TProviderORMBr<T>.Destroy;
begin
  FManager.Free;
  FProviderDM.Free;
  inherited;
end;

function TProviderORMBr<T>.Execute(const AURL: String; const ARequestMethod: TRESTRequestMethodType;
  const AParamsProc: TProc): String;
begin
  Result := ProviderDM.RESTClientHorse1.Execute(AURL, ARequestMethod, AParamsProc);
end;

function TProviderORMBr<T>.DataSet<D>: TDataSet;
begin
  Result := FManager.DataSet<D>;
end;

procedure TProviderORMBr<T>.MonitorShow;
begin
  TCommandMonitor.GetInstance.Show;
end;

procedure TProviderORMBr<T>.Open;
begin
  FManager.Open<T>;
end;

procedure TProviderORMBr<T>.OpenWhere(const AWhere: String);
begin
  FManager.OpenWhere<T>(AWhere);
end;

function TProviderORMBr<T>.ProviderDM: TProviderDM;
begin
  Result := FProviderDM;
end;

end.

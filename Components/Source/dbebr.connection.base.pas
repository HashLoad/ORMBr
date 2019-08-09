unit dbebr.connection.base;

interface

uses
  DB,
  Classes,
  ormbr.factory.interfaces;

type
  {$IF CompilerVersion > 23}
  [ComponentPlatformsAttribute(pidWin32 or
                               pidWin64 or
                               pidOSX32 or
                               pidiOSSimulator or
                               pidiOSDevice or
                               pidAndroid)]
  {$IFEND}
  TDBEBrConnectionBase = class(TComponent)
  protected
    FDBConnection: IDBConnection;
    FDriverName: TDriverName;
    function GetDBConnection: IDBConnection; virtual; abstract;
  public
    procedure Connect;
    procedure Disconnect;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    procedure ExecuteDirect(const ASQL: string); overload;
    procedure ExecuteDirect(const ASQL: string; const AParams: TParams); overload;
    procedure ExecuteScript(const ASQL: string);
    procedure AddScript(const ASQL: string);
    procedure ExecuteScripts;
    procedure SetCommandMonitor(AMonitor: ICommandMonitor);
    function InTransaction: Boolean;
    function IsConnected: Boolean;
    function CreateQuery: IDBQuery;
    function CreateResultSet(const ASQL: String): IDBResultSet;
    function ExecuteSQL(const ASQL: string): IDBResultSet;
    function CommandMonitor: ICommandMonitor;
    function Connection: IDBConnection;
  published
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    property DriverName: TDriverName read FDriverName write FDriverName;
  end;

implementation

{ TDBEBrConnectionBase }

constructor TDBEBrConnectionBase.Create(AOwner: TComponent);
begin

end;

destructor TDBEBrConnectionBase.Destroy;
begin

  inherited;
end;

procedure TDBEBrConnectionBase.AddScript(const ASQL: string);
begin
  GetDBConnection.AddScript(ASQL);
end;

function TDBEBrConnectionBase.CommandMonitor: ICommandMonitor;
begin
  Result := GetDBConnection.CommandMonitor;
end;

procedure TDBEBrConnectionBase.Commit;
begin
  GetDBConnection.Commit;
end;

procedure TDBEBrConnectionBase.Connect;
begin
  GetDBConnection.Connect;
end;

function TDBEBrConnectionBase.Connection: IDBConnection;
begin
  Result := GetDBConnection;
end;

function TDBEBrConnectionBase.CreateQuery: IDBQuery;
begin
  Result := GetDBConnection.CreateQuery;
end;

function TDBEBrConnectionBase.CreateResultSet(
  const ASQL: String): IDBResultSet;
begin
  Result := GetDBConnection.CreateResultSet(ASQL);
end;

procedure TDBEBrConnectionBase.Disconnect;
begin
  GetDBConnection.Disconnect;
end;

procedure TDBEBrConnectionBase.ExecuteDirect(const ASQL: string);
begin
  GetDBConnection.ExecuteDirect(ASQL);
end;

procedure TDBEBrConnectionBase.ExecuteDirect(const ASQL: string;
  const AParams: TParams);
begin
  GetDBConnection.ExecuteDirect(ASQL, AParams);
end;

procedure TDBEBrConnectionBase.ExecuteScript(const ASQL: string);
begin
  GetDBConnection.ExecuteScript(ASQL);
end;

procedure TDBEBrConnectionBase.ExecuteScripts;
begin
  GetDBConnection.ExecuteScripts;
end;

function TDBEBrConnectionBase.ExecuteSQL(const ASQL: string): IDBResultSet;
begin
  Result := GetDBConnection.ExecuteSQL(ASQL);
end;

function TDBEBrConnectionBase.InTransaction: Boolean;
begin
  Result := GetDBConnection.InTransaction;
end;

function TDBEBrConnectionBase.IsConnected: Boolean;
begin
  Result := GetDBConnection.IsConnected;
end;

procedure TDBEBrConnectionBase.Rollback;
begin
  GetDBConnection.Rollback;
end;

procedure TDBEBrConnectionBase.SetCommandMonitor(AMonitor: ICommandMonitor);
begin
  GetDBConnection.SetCommandMonitor(AMonitor);
end;

procedure TDBEBrConnectionBase.StartTransaction;
begin
  GetDBConnection.StartTransaction;
end;

end.

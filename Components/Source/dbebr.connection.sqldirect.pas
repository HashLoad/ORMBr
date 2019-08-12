unit dbebr.connection.sqldirect;

interface

uses
  DB,
  Classes,
  SDEngine,
  dbebr.connection.base,
  ormbr.factory.sqldirect,
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
  TDBEBrConnectionSQLDirect = class(TDBEBrConnectionBase)
  private
    FConnection: TSDDatabase;
  public
    constructor Create(const AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDBConnection: IDBConnection; override;
  published
    property Connetion: TSDDatabase read FConnection write FConnection;
  end;

implementation

{ TDBEBrConnectionSQLDirect }

constructor TDBEBrConnectionSQLDirect.Create(const AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDBEBrConnectionSQLDirect.Destroy;
begin

  inherited;
end;

function TDBEBrConnectionSQLDirect.GetDBConnection: IDBConnection;
begin
  if not Assigned(FDBConnection) then
    FDBConnection := TFactorySQLDirect.Create(FConnection, FDriverName);
  Result := FDBConnection;
end;

end.

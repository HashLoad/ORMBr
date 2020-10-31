unit dbebr.connection.zeos;

interface

uses
  DB,
  Classes,
  ZConnection,
  dbebr.connection.base,
  dbebr.factory.zeos,
  dbebr.factory.interfaces;

type
  {$IF CompilerVersion > 23}
  [ComponentPlatformsAttribute(pidWin32 or
                               pidWin64 or
                               pidOSX32 or
                               pidiOSSimulator or
                               pidiOSDevice or
                               pidAndroid)]
  {$IFEND}
  TDBEBrConnectionZeos = class(TDBEBrConnectionBase)
  private
    FConnection: TZConnection;
    procedure SetConnection(const Value: TZConnection);
    function GetConnection: TZConnection;
  public
    constructor Create(const AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Connection: TZConnection read GetConnection write SetConnection;
  end;

implementation

{ TDBEBrConnectionZeos }

constructor TDBEBrConnectionZeos.Create(const AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDBEBrConnectionZeos.Destroy;
begin

  inherited;
end;

function TDBEBrConnectionZeos.GetConnection: TZConnection;
begin
  Result := FConnection;
end;

procedure TDBEBrConnectionZeos.SetConnection(const Value: TZConnection);
begin
  FConnection := Value;
  if not Assigned(FDBConnection) then
    FDBConnection := TFactoryZeos.Create(FConnection, FDriverName);
end;

end.

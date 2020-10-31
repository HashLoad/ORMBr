unit dbebr.connection.nexusdb;

interface

uses
  DB,
  Classes,
  nxdb,
  nxllComponent,
  dbebr.connection.base,
  dbebr.factory.nexusdb,
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
  TDBEBrConnectionNexusDB = class(TDBEBrConnectionBase)
  private
    FConnection: TnxDatabase;
    procedure SetConnection(const Value: TnxDatabase);
    function GetConnection: TnxDatabase;
  public
    constructor Create(const AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Connection: TnxDatabase read GetConnection write SetConnection;
  end;

implementation

{ TDBEBrConnectionNexusDB }

constructor TDBEBrConnectionNexusDB.Create(const AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDBEBrConnectionNexusDB.Destroy;
begin

  inherited;
end;

function TDBEBrConnectionNexusDB.GetConnection: TnxDatabase;
begin
  Result := FConnection;
end;

procedure TDBEBrConnectionNexusDB.SetConnection(const Value: TnxDatabase);
begin
  FConnection := Value;
  if not Assigned(FDBConnection) then
    FDBConnection := TFactoryNexusDB.Create(FConnection, FDriverName);
end;

end.

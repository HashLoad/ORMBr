unit dbebr.connection.nexusdb;

interface

uses
  DB,
  Classes,
  nxdb,
  nxllComponent,
  dbebr.connection.base,
  ormbr.factory.nexusdb,
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
  TDBEBrConnectionNexusDB = class(TDBEBrConnectionBase)
  private
    FConnection: TnxDatabase;
  public
    constructor Create(const AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDBConnection: IDBConnection; override;
  published
    property Connetion: TnxDatabase read FConnection write FConnection;
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

function TDBEBrConnectionNexusDB.GetDBConnection: IDBConnection;
begin
  if not Assigned(FDBConnection) then
    FDBConnection := TFactoryNexusDB.Create(FConnection, FDriverName);
  Result := FDBConnection;
end;

end.

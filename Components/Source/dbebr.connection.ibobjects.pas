unit dbebr.connection.ibobjects;

interface

uses
  DB,
  Classes,
  IB_Components,
  IBODataset,
  IB_Access,
  dbebr.connection.base,
  dbebr.factory.ibobjects,
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
  TDBEBrConnectionIBObjects = class(TDBEBrConnectionBase)
  private
    FConnection: TIBODatabase;
    procedure SetConnection(const Value: TIBODatabase);
    function GetConnection: TIBODatabase;
  public
    constructor Create(const AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Connection: TIBODatabase read GetConnection write SetConnection;
  end;

implementation

{ TDBEBrConnectionIBObjects }

constructor TDBEBrConnectionIBObjects.Create(const AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDBEBrConnectionIBObjects.Destroy;
begin

  inherited;
end;

function TDBEBrConnectionIBObjects.GetConnection: TIBODatabase;
begin
  Result := FConnection;
end;

procedure TDBEBrConnectionIBObjects.SetConnection(const Value: TIBODatabase);
begin
  FConnection := Value;
  if not Assigned(FDBConnection) then
    FDBConnection := TFactoryIBObjects.Create(FConnection, FDriverName);
end;

end.

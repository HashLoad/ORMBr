unit dbebr.connection.fibplus;

interface

uses
  DB,
  Classes,
  FIBQuery,
  FIBDataSet,
  FIBDatabase,
  dbebr.connection.base,
  dbebr.factory.fibplus,
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
  TDBEBrConnectionFIBPlus = class(TDBEBrConnectionBase)
  private
    FConnection: TFIBDatabase;
    procedure SetConnection(const Value: TFIBDatabase);
    function GetConnection: TFIBDatabase;
  public
    constructor Create(const AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Connection: TFIBDatabase read GetConnection write SetConnection;
  end;

implementation

{ TDBEBrConnectionFIBPlus }

constructor TDBEBrConnectionFIBPlus.Create(const AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDBEBrConnectionFIBPlus.Destroy;
begin

  inherited;
end;

function TDBEBrConnectionFIBPlus.GetConnection: TFIBDatabase;
begin
  Result := FConnection;
end;

procedure TDBEBrConnectionFIBPlus.SetConnection(const Value: TFIBDatabase);
begin
  FConnection := Value;
  if not Assigned(FDBConnection) then
    FDBConnection := TFactoryFIBPlus.Create(FConnection, FDriverName);
end;

end.

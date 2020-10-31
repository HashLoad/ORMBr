unit dbebr.connection.ado;

interface

uses
  DB,
  Classes,
  ADODB,
  dbebr.connection.base,
  dbebr.factory.ado,
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
  TDBEBrConnectionADO = class(TDBEBrConnectionBase)
  private
    FConnection: TADOConnection;
    procedure SetConnection(const Value: TADOConnection);
    function GetConnection: TADOConnection;
  public
    constructor Create(const AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Connection: TADOConnection read GetConnection write SetConnection;
  end;

implementation

{ TDBEBrConnectionADO }

constructor TDBEBrConnectionADO.Create(const AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDBEBrConnectionADO.Destroy;
begin

  inherited;
end;

function TDBEBrConnectionADO.GetConnection: TADOConnection;
begin
  Result := FConnection;
end;

procedure TDBEBrConnectionADO.SetConnection(const Value: TADOConnection);
begin
  FConnection := Value;
  if not Assigned(FDBConnection) then
    FDBConnection := TFactoryADO.Create(FConnection, FDriverName);
end;

end.

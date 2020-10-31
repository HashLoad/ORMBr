unit dbebr.connection.unidac;

interface

uses
  DB,
  Classes,
  Uni,
  dbebr.connection.base,
  dbebr.factory.unidac,
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
  TDBEBrConnectionUniDAC = class(TDBEBrConnectionBase)
  private
    FConnection: TUniConnection;
    procedure SetConnection(const Value: TUniConnection);
    function GetConnection: TUniConnection;
  public
    constructor Create(const AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Connection: TUniConnection read GetConnection write SetConnection;
  end;

implementation

{ TDBEBrConnectionUniDAC }

constructor TDBEBrConnectionUniDAC.Create(const AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDBEBrConnectionUniDAC.Destroy;
begin

  inherited;
end;

function TDBEBrConnectionUniDAC.GetConnection: TUniConnection;
begin
  Result := FConnection;
end;

procedure TDBEBrConnectionUniDAC.SetConnection(const Value: TUniConnection);
begin
  FConnection := Value;
  if not Assigned(FDBConnection) then
    FDBConnection := TFactoryUniDAC.Create(FConnection, FDriverName);
end;

end.

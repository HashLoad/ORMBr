unit dbebr.connection.firedac;

interface

uses
  DB,
  Classes,
  FireDAC.Comp.Client,
  dbebr.connection.base,
  dbebr.factory.firedac,
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
  TDBEBrConnectionFireDAC = class(TDBEBrConnectionBase)
  private
    FConnection: TFDConnection;
    procedure SetConnection(const Value: TFDConnection);
    function GetConnection: TFDConnection;
  public
    constructor Create(const AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Connection: TFDConnection read GetConnection write SetConnection;
  end;

implementation

{ TDBEBrConnectionFireDAC }

constructor TDBEBrConnectionFireDAC.Create(const AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDBEBrConnectionFireDAC.Destroy;
begin

  inherited;
end;

function TDBEBrConnectionFireDAC.GetConnection: TFDConnection;
begin
  Result := FConnection;
end;

procedure TDBEBrConnectionFireDAC.SetConnection(const Value: TFDConnection);
begin
  FConnection := Value;
  if not Assigned(FDBConnection) then
    FDBConnection := TFactoryFireDAC.Create(FConnection, FDriverName);
end;

end.

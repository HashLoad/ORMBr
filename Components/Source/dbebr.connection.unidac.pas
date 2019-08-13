unit dbebr.connection.unidac;

interface

uses
  DB,
  Classes,
  Uni,
  dbebr.connection.base,
  ormbr.factory.unidac,
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
  TDBEBrConnectionUniDAC = class(TDBEBrConnectionBase)
  private
    FConnection: TUniConnection;
  public
    constructor Create(const AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDBConnection: IDBConnection; override;
  published
    property Connetion: TUniConnection read FConnection write FConnection;
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

function TDBEBrConnectionUniDAC.GetDBConnection: IDBConnection;
begin
  if not Assigned(FDBConnection) then
    FDBConnection := TFactoryUniDAC.Create(FConnection, FDriverName);
  Result := FDBConnection;
end;

end.

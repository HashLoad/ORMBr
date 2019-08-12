unit dbebr.connection.zeos;

interface

uses
  DB,
  Classes,
  ZConnection,
  dbebr.connection.base,
  ormbr.factory.zeos,
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
  TDBEBrConnectionZeos = class(TDBEBrConnectionBase)
  private
    FConnection: TZConnection;
  public
    constructor Create(const AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDBConnection: IDBConnection; override;
  published
    property Connetion: TZConnection read FConnection write FConnection;
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

function TDBEBrConnectionZeos.GetDBConnection: IDBConnection;
begin
  if not Assigned(FDBConnection) then
    FDBConnection := TFactoryZeos.Create(FConnection, FDriverName);
  Result := FDBConnection;
end;

end.

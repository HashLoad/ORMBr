unit dbebr.connection.fibplus;

interface

uses
  DB,
  Classes,
  FIBQuery,
  FIBDataSet,
  FIBDatabase,
  dbebr.connection.base,
  ormbr.factory.fibplus,
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
  TDBEBrConnectionFIBPlus = class(TDBEBrConnectionBase)
  private
    FConnection: TFIBDatabase;
  public
    function GetDBConnection: IDBConnection; override;
  published
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Connetion: TFIBDatabase read FConnection write FConnection;
  end;

implementation

{ TDBEBrConnectionFIBPlus }

constructor TDBEBrConnectionFIBPlus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDBEBrConnectionFIBPlus.Destroy;
begin

  inherited;
end;

function TDBEBrConnectionFIBPlus.GetDBConnection: IDBConnection;
begin
  if not Assigned(FDBConnection) then
    FDBConnection := TFactoryFIBPlus.Create(FConnection, FDriverName);
  Result := FDBConnection;
end;

end.

unit dbebr.connection.ibobjects;

interface

uses
  DB,
  Classes,
  IB_Components,
  IBODataset,
  IB_Access,
  dbebr.connection.base,
  ormbr.factory.ibobjects,
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
  TDBEBrConnectionIBObjects = class(TDBEBrConnectionBase)
  private
    FConnection: TIBODatabase;
  public
    constructor Create(const AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDBConnection: IDBConnection; override;
  published
    property Connetion: TIBODatabase read FConnection write FConnection;
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

function TDBEBrConnectionIBObjects.GetDBConnection: IDBConnection;
begin
  if not Assigned(FDBConnection) then
    FDBConnection := TFactoryIBObjects.Create(FConnection, FDriverName);
  Result := FDBConnection;
end;

end.

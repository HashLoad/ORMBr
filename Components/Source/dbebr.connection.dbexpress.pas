unit dbebr.connection.dbexpress;

interface

uses
  DB,
  SqlExpr,
  Classes,
  dbebr.connection.base,
  ormbr.factory.dbexpress,
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
  TDBEBrConnectionDBExpress = class(TDBEBrConnectionBase)
  private
    FConnection: TSQLConnection;
  public
    function GetDBConnection: IDBConnection; Override;
  published
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Connetion: TSQLConnection read FConnection write FConnection;
  end;

implementation

{ TDBEBrConnectionDBExpress }

constructor TDBEBrConnectionDBExpress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDBEBrConnectionDBExpress.Destroy;
begin

  inherited;
end;

function TDBEBrConnectionDBExpress.GetDBConnection: IDBConnection;
begin
  if not Assigned(FDBConnection) then
    FDBConnection := TFactoryDBExpress.Create(FConnection, FDriverName);
  Result := FDBConnection;
end;

end.

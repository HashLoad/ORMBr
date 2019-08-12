unit dbebr.connection.ado;

interface

uses
  DB,
  Classes,
  ADODB,
  dbebr.connection.base,
  ormbr.factory.ado,
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
  TDBEBrConnectionADO = class(TDBEBrConnectionBase)
  private
    FConnection: TADOConnection;
  public
    constructor Create(const AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDBConnection: IDBConnection; override;
  published
    property Connetion: TADOConnection read FConnection write FConnection;
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

function TDBEBrConnectionADO.GetDBConnection: IDBConnection;
begin
  if not Assigned(FDBConnection) then
    FDBConnection := TFactoryADO.Create(FConnection, FDriverName);
  Result := FDBConnection;
end;

end.

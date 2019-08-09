unit dbebr.connection.firedac;

interface

uses
  DB,
  Classes,
  FireDAC.Comp.Client,
  dbebr.connection.base,
  ormbr.factory.firedac,
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
  TDBEBrConnectionFireDAC = class(TDBEBrConnectionBase)
  private
    FConnection: TFDConnection;
  public
    function GetDBConnection: IDBConnection; override;
  published
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Connetion: TFDConnection read FConnection write FConnection;
  end;

implementation

{ TDBEBrConnectionFireDAC }

constructor TDBEBrConnectionFireDAC.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDBEBrConnectionFireDAC.Destroy;
begin

  inherited;
end;

function TDBEBrConnectionFireDAC.GetDBConnection: IDBConnection;
begin
  if not Assigned(FDBConnection) then
    FDBConnection := TFactoryFireDAC.Create(FConnection, FDriverName);
  Result := FDBConnection;
end;

end.

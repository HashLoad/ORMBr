unit MongoWireConnection;

interface

uses
  SysUtils,
  Classes,
  mongoWire,
  jsonDoc,
  mongoAuth3;

type
  TMongoWireConnection = class(TComponent)
  private
    FMongoWire: TMongoWire;
    FDatabase: String;
    FHost: String;
    FPort: Integer;
    FConnected: Boolean;
    FUser : String;
    FPassword : String;
    FAuthenticate : Boolean;
    procedure SetConnected(const Value: Boolean);
    function GetConnected: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RunCommand(ADoc: String): IJSONDocument;
    function MongoWire: TMongoWire;
  published
    property Database: String read FDatabase write FDatabase;
    property Host: String read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property Connected: Boolean read GetConnected write SetConnected;
    property User : String read FUser write FUser;
    property Password : String read FPassword write FPassword;
    Property Authenticate : Boolean read FAuthenticate write FAuthenticate;
  end;

procedure Register;

implementation

{$R 'MongoWireConnection.res' 'MongoWireConnection.rc'}

procedure Register;
begin
  RegisterComponents('ORMBr-NoSQL', [TMongoWireConnection]);
end;

constructor TMongoWireConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDatabase := 'database';
  FHost := 'localhost';
  FPort := 27017;

  FMongoWire := TMongoWire.Create(FDatabase);
end;

destructor TMongoWireConnection.Destroy;
begin
  if Assigned(FMongoWire) then
    FMongoWire.DisposeOf;
  inherited;
end;

function TMongoWireConnection.GetConnected: Boolean;
begin
  Result := FConnected;
end;

function TMongoWireConnection.MongoWire: TMongoWire;
begin
  Result := FMongoWire;
end;

function TMongoWireConnection.RunCommand(ADoc: String): IJSONDocument;
var
  LDoc: IJSONDocument;
begin
  LDoc := JSON(ADoc);
  Result := FMongoWire.RunCommand(LDoc);
end;

procedure TMongoWireConnection.SetConnected(const Value: Boolean);
begin
  FConnected := Value;
  if FConnected then
  begin
    FMongoWire.Open(FHost, FPort);
    if FAuthenticate then
      MongoWireAuthenticate(FMongoWire, User, Password);
  end
  else
    FMongoWire.Close;
end;

end.


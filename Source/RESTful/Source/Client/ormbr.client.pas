{
      ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2018, Isaque Pinheiro
                          All rights reserved.
}

{
  @abstract(REST Componentes)
  @created(20 Jun 2018)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)
}

unit ormbr.client;

interface

uses
  DB,
  SysUtils,
  StrUtils,
  Classes,
  ormbr.client.methods,
  ormbr.client.base;

type
  TClientParam = array of String;
  PClientParam = ^TClientParam;

  TAuthentication = procedure of object;
  TBeforeCommandEvent = procedure (ARequestMethod: String) of object;
  TAfterCommandEvent = procedure (AStatusCode: Integer;
                              var AResponseString: String;
                                  ARequestMethod: String) of object;
  TErrorCommandEvent = procedure (const AURLBase: String;
                                  const AResource: String;
                                  const ASubResource: String;
                                  const ARequestMethod: String;
                                  const AMessage: String;
                                  const AResponseCode: Integer) of object;

  TRestProtocol = (Http, Https);

  TORMBrClient = class(TORMBrClientBase)
  private
    FBeforeCommand: TBeforeCommandEvent;
    FAfterCommand: TAfterCommandEvent;
    function GetMethodGET: String;
    procedure SetMethodGET(const Value: String);
    function GetMethodGETId: String;
    procedure SetMethodGETId(const Value: String);
    function GetMethodGETWhere: String;
    procedure SetMethodGETWhere(const Value: String);
    function GetMethodPOST: String;
    procedure SetMethodPOST(const Value: String);
    function GetMethodPUT: String;
    procedure SetMethodPUT(const Value: String);
    function GetMethodDELETE: String;
    procedure SetMethodDELETE(const Value: String);
    function GetMethodGETNextPacketWhere: String;
    procedure SetMethodGETNextPacketWhere(const Value: String);
    function GetMethodGETNextPacket: String;
    procedure SetMethodGETNextPacket(const Value: String);
    function GetMethodToken: String;
    procedure SetMethodToken(const Value: String);
    function GetHost: String;
    procedure SetHost(const Value: String);
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetAPIContext: String;
    procedure SetAPIContext(const Value: String);
    function GetRESTContext: String;
    procedure SetRESTContext(const Value: String);
    function GetProtocol: TRestProtocol;
    procedure SetProtocol(const Value: TRestProtocol);
  protected
    FErrorCommand: TErrorCommandEvent;
    FProtocol: TRestProtocol;
    FParams: TParams;
    FBodyParams: TParams;
    FQueryParams: TParams;
    FBaseURL: String;
    FAPIContext: String;
    FRESTContext: String;
    FHost: String;
    FPort: Integer;
    FServerUse: Boolean;
    FClassNotServerUse: Boolean;
    // Variável de controle, para conseguir chamar o método Execute()
    // de dentro do evento de autenticação.
    FPerformingAuthentication: Boolean;
    FMethodSelect: String;
    FMethodSelectID: String;
    FMethodSelectWhere: String;
    FMethodInsert: String;
    FMethodUpdate: String;
    FMethodDelete: String;
    FMethodNextPacket: String;
    FMethodNextPacketWhere: String;
    FMethodToken: String;
    // Variables the Events
    FRequestMethod: String;
    FResponseString: String;
    FStatusCode: Integer;
    FAuthentication: TAuthentication;
    procedure SetServerUse(const Value: Boolean); virtual;
    procedure SetBaseURL; virtual;
    function GetBaseURL: String;
    function GetFullURL: String; virtual;
    procedure DoBeforeCommand; virtual;
    procedure DoAfterCommand; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetClassNotServerUse(const Value: Boolean);
    procedure AddParam(AValue: String); virtual;
    procedure AddBodyParam(AValue: String); virtual;
    procedure AddQueryParam(AValue: String); virtual;
    property MethodGET: String read GetMethodGET write SetMethodGET;
    property MethodPOST: String read GetMethodPOST write SetMethodPOST;
    property MethodPUT: String read GetMethodPUT write SetMethodPUT;
    property MethodDELETE: String read GetMethodDELETE write SetMethodDELETE;
    property MethodToken: String read GetMethodToken write SetMethodToken;
    property APIContext: String read GetAPIContext write SetAPIContext;
    property RESTContext: String read GetRESTContext write SetRESTContext;
    property ORMBrServerUse: Boolean read FServerUse write SetServerUse;
  published
    property Protocol: TRestProtocol read GetProtocol write SetProtocol;
    property Host: String read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort;
    property MethodGETId: String read GetMethodGETId write SetMethodGETId;
    property MethodGETWhere: String read GetMethodGETWhere write SetMethodGETWhere;
    property MethodGETNextPacket: String read GetMethodGETNextPacket write SetMethodGETNextPacket;
    property MethodGETNextPacketWhere: String read GetMethodGETNextPacketWhere write SetMethodGETNextPacketWhere;
    property BaseURL: String read GetBaseURL;
    property FullURL: String read GetFullURL;
    property OnAuthentication: TAuthentication read FAuthentication write FAuthentication;
    property OnBeforeCommand: TBeforeCommandEvent read FBeforeCommand write FBeforeCommand;
    property OnAfterCommand: TAfterCommandEvent read FAfterCommand write FAfterCommand;
    property OnErrorCommand: TErrorCommandEvent read FErrorCommand write FErrorCommand;
  end;

implementation

{ TORMBrClient }

procedure TORMBrClient.AddQueryParam(AValue: String);
begin
  with FQueryParams.Add as TParam do
  begin
    Name := 'param_' + IntToStr(FQueryParams.Count -1);
    DataType := ftString;
    ParamType := ptInput;
    Value := AValue;
  end;
end;

constructor TORMBrClient.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF TRIAL}
  try
    raise Exception.Create('Esta é uma versão de demonstração do ORMBr - REST Client Components. Adquira a versão completa pelo E-mail ormbrframework@gmail.com');
  except end;
  {$ENDIF}
  FParams := TParams.Create(Self);
  FBodyParams := TParams.Create(Self);
  FQueryParams := TParams.Create(Self);
  FServerUse := False;
  FClassNotServerUse := False;
  FPerformingAuthentication := False;
  FHost := 'localhost';
  FPort := 8080;
  FMethodSelect := '';
  FMethodInsert := '';
  FMethodUpdate := '';
  FMethodDelete := '';
  FMethodSelectID := 'selectid';
  FMethodSelectWhere := 'selectwhere';
  FMethodNextPacket := 'nextpacket';
  FMethodNextPacketWhere := 'nextpacketwhere';
  FMethodToken := 'token';
  FAPIContext := '';
  FRESTContext := '';
  FProtocol := TRestProtocol.Http;
  FResponseString := '';
  FRequestMethod := '';
  FStatusCode := 0;
  // Monta a URL base
  SetBaseURL;
end;

destructor TORMBrClient.Destroy;
begin
  FParams.Clear;
  FParams.Free;
  FQueryParams.Clear;
  FQueryParams.Free;
  FBodyParams.Clear;
  FBodyParams.Free;
  inherited;
end;

procedure TORMBrClient.DoAfterCommand;
begin
  if Assigned(FAfterCommand) then
    FAfterCommand(FStatusCode, FResponseString, FRequestMethod);
end;

procedure TORMBrClient.DoBeforeCommand;
begin
  if Assigned(FBeforeCommand) then
    FBeforeCommand(FRequestMethod);
end;

procedure TORMBrClient.AddBodyParam(AValue: String);
begin
  with FBodyParams.Add as TParam do
  begin
    Name := 'body';
    DataType := ftString;
    ParamType := ptInput;
    Value := AValue;
  end;
end;

procedure TORMBrClient.AddParam(AValue: String);
begin
  with FParams.Add as TParam do
  begin
    Name := 'param_' + IntToStr(FParams.Count -1);
    DataType := ftString;
    ParamType := ptInput;
    Value := AValue;
  end;
end;

procedure TORMBrClient.SetBaseURL;
var
  LProtocol: String;
begin
  LProtocol := ifThen(FProtocol = TRestProtocol.Http, 'http://', 'https://');
  FBaseURL := LProtocol + FHost;
  if FPort > 0 then
    FBaseURL := FBaseURL + ':' + IntToStr(FPort) + '/';
end;

procedure TORMBrClient.SetClassNotServerUse(const Value: Boolean);
begin
  FClassNotServerUse := Value;
end;

function TORMBrClient.GetBaseURL: String;
begin
  Result := FBaseURL;
end;

function TORMBrClient.GetFullURL: String;
begin
  Result := FBaseURL;
end;

function TORMBrClient.GetAPIContext: String;
begin
  Result := FAPIContext;
end;

function TORMBrClient.GetMethodDELETE: String;
begin
  Result := FMethodDelete;
end;

function TORMBrClient.GetHost: String;
begin
  Result := FHost;
end;

function TORMBrClient.GetMethodPOST: String;
begin
  Result := FMethodInsert;
end;

function TORMBrClient.GetMethodGETNextPacket: String;
begin
  Result := FMethodNextPacket;
end;

function TORMBrClient.GetMethodGETNextPacketWhere: String;
begin
  Result := FMethodNextPacketWhere;
end;

function TORMBrClient.GetPort: Integer;
begin
  Result := FPort;
end;

function TORMBrClient.GetProtocol: TRestProtocol;
begin
  Result := FProtocol;
end;

function TORMBrClient.GetRESTContext: String;
begin
  Result := FRESTContext;
end;

function TORMBrClient.GetMethodGET: String;
begin
  Result := FMethodSelect;
end;

function TORMBrClient.GetMethodGETId: String;
begin
  Result := FMethodSelectID;
end;

function TORMBrClient.GetMethodGETWhere: String;
begin
  Result := FMethodSelectWhere;
end;

function TORMBrClient.GetMethodToken: String;
begin
  Result := FMethodToken;
end;

function TORMBrClient.GetMethodPUT: String;
begin
  Result := FMethodUpdate;
end;

procedure TORMBrClient.SetAPIContext(const Value: String);
begin
  if FAPIContext = Value then
    Exit;

  FAPIContext := Value;
  // Monta a URL base
  SetBaseURL;
end;

procedure TORMBrClient.SetMethodDELETE(const Value: String);
begin
  if FMethodDelete <> Value then
    FMethodDelete := Value;
end;

procedure TORMBrClient.SetHost(const Value: String);
begin
  if FHost = Value then
    Exit;

  FHost := Value;
  // Monta a URL base
  SetBaseURL;
end;

procedure TORMBrClient.SetMethodPOST(const Value: String);
begin
  if FMethodInsert <> Value then
    FMethodInsert := Value;
end;

procedure TORMBrClient.SetMethodGETNextPacket(const Value: String);
begin
  if FMethodNextPacket <> Value then
    FMethodNextPacket := Value;
end;

procedure TORMBrClient.SetMethodGETNextPacketWhere(const Value: String);
begin
  if FMethodNextPacketWhere <> Value then
    FMethodNextPacketWhere := Value;
end;

procedure TORMBrClient.SetPort(const Value: Integer);
begin
  if FPort = Value then
    Exit;

  FPort := Value;
  // Monta a URL base
  SetBaseURL;
end;

procedure TORMBrClient.SetProtocol(const Value: TRestProtocol);
begin
  if FProtocol = Value then
    Exit;

  FProtocol := Value;
  // Monta a URL base
  SetBaseURL;
end;

procedure TORMBrClient.SetRESTContext(const Value: String);
begin
  if FRESTContext = Value then
    Exit;

  FRESTContext := Value;
  // Monta a URL base
  SetBaseURL;
end;

procedure TORMBrClient.SetServerUse(const Value: Boolean);
begin
  if FServerUse = Value then
    Exit;

  FServerUse := Value;
  if FServerUse then
  begin
    if Pos('/ORMBR', UpperCase(FAPIContext)) = 0 then
      FAPIContext := FAPIContext + '/ormbr';
  end
  else
    FAPIContext := ReplaceStr(FAPIContext, '/ormbr', '');
end;

procedure TORMBrClient.SetMethodGET(const Value: String);
begin
  if FMethodSelect <> Value then
    FMethodSelect := Value;
end;

procedure TORMBrClient.SetMethodGETId(const Value: String);
begin
  if FMethodSelectID <> Value then
    FMethodSelectID := Value;
end;

procedure TORMBrClient.SetMethodGETWhere(const Value: String);
begin
  if FMethodSelectWhere <> Value then
    FMethodSelectWhere := Value;
end;

procedure TORMBrClient.SetMethodToken(const Value: String);
begin
  if FMethodToken <> Value then
    FMethodToken := Value;
end;

procedure TORMBrClient.SetMethodPUT(const Value: String);
begin
  if FMethodUpdate <> Value then
    FMethodUpdate := Value;
end;

end.

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

unit ormbr.client.restdriver.horse;

interface

uses
  Classes,
  SysUtils,
  ormbr.client.horse,
  ormbr.client.restdriver,
  ormbr.client.methods;

type
  // Classe de conexão concreta com dbExpress
  TRESTDriverHorse = class(TRESTDriver)
  protected
    FConnection: TRESTClientHorse;
  public
    constructor Create(AConnection: TComponent); override;
    destructor Destroy; override;
    function GetBaseURL: String; override;
    function GetFullURL: String; override;
    function GetMethodGET: String; override;
    function GetMethodGETId: String; override;
    function GetMethodGETWhere: String; override;
    function GetMethodPOST: String; override;
    function GetMethodPUT: String; override;
    function GetMethodDELETE: String; override;
    function GetMethodGETNextPacket: String; override;
    function GetMethodGETNextPacketWhere: String; override;
    function GetMethodToken: String; override;
    function GetServerUse: Boolean; override;
    function Execute(const AResource, ASubResource: String;
      const ARequestMethod: TRESTRequestMethodType;
      const AParams: TProc = nil): String; overload; override;
    function GetUsername: String; override;
    function GetPassword: String; override;
    procedure SetClassNotServerUse(const Value: Boolean); override;
    procedure AddParam(const AValue: String); override;
    procedure AddQueryParam(const AValue: String); override;
    procedure AddBodyParam(const AValue: String); override;
  end;

implementation

{ TDriverRestHorse }

procedure TRESTDriverHorse.AddBodyParam(const AValue: String);
begin
  inherited;
  FConnection.AddBodyParam(AValue);
end;

procedure TRESTDriverHorse.AddParam(const AValue: String);
begin
  inherited;
  FConnection.AddParam(AValue);
end;

procedure TRESTDriverHorse.AddQueryParam(const AValue: String);
begin
  inherited;
  FConnection.AddQueryParam(AValue);
end;

constructor TRESTDriverHorse.Create(AConnection: TComponent);
begin
  inherited;
  FConnection := AConnection as TRESTClientHorse;
end;

destructor TRESTDriverHorse.Destroy;
begin
  FConnection := nil;
  inherited;
end;

function TRESTDriverHorse.Execute(const AResource, ASubResource: String;
  const ARequestMethod: TRESTRequestMethodType; const AParams: TProc): String;
begin
  Result := FConnection
              .Execute(AResource, ASubResource, ARequestMethod, AParams);
end;

function TRESTDriverHorse.GetBaseURL: String;
begin
  Result := FConnection.BaseURL;
end;

function TRESTDriverHorse.GetFullURL: String;
begin
  Result := FConnection.FullURL;
end;

function TRESTDriverHorse.GetMethodDELETE: String;
begin
  Result := FConnection.MethodDelete;
end;

function TRESTDriverHorse.GetMethodPOST: String;
begin
  Result := FConnection.MethodPOST;
end;

function TRESTDriverHorse.GetMethodGETNextPacket: String;
begin
  Result := FConnection.MethodGETNextPacket;
end;

function TRESTDriverHorse.GetMethodGETNextPacketWhere: String;
begin
  Result := FConnection.MethodGETNextPacketWhere;
end;

function TRESTDriverHorse.GetMethodGET: String;
begin
  Result := FConnection.MethodGET;
end;

function TRESTDriverHorse.GetMethodGETId: String;
begin
  Result := FConnection.MethodGETId;
end;

function TRESTDriverHorse.GetMethodGETWhere: String;
begin
  Result := FConnection.MethodGETWhere;
end;

function TRESTDriverHorse.GetMethodPUT: String;
begin
  Result := FConnection.MethodPUT;
end;

function TRESTDriverHorse.GetMethodToken: String;
begin
  Result := FConnection.Authenticator.Token;
end;

function TRESTDriverHorse.GetPassword: String;
begin
  Result := FConnection.Authenticator.Password;
end;

function TRESTDriverHorse.GetServerUse: Boolean;
begin
  Result := FConnection.ORMBrServerUse;
end;

function TRESTDriverHorse.GetUsername: String;
begin
  Result := FConnection.Authenticator.Username;
end;

procedure TRESTDriverHorse.SetClassNotServerUse(const Value: Boolean);
begin
  FConnection.SetClassNotServerUse(Value);
end;

end.

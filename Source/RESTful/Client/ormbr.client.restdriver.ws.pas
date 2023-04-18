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

unit ormbr.client.restdriver.ws;

interface

uses
  Classes,
  SysUtils,
  ormbr.client.ws,
  ormbr.client.restdriver,
  ormbr.client.methods;

type
  /// <summary>
  /// Classe de conexão concreta com dbExpress
  /// </summary>
  TRESTDriverWS = class(TRESTDriver)
  protected
    FConnection: TRESTClientWS;
  public
    constructor Create(AConnection: TComponent); override;
    destructor Destroy; override;
    function GetBaseURL: String; override;
    function GetMethodGET: String; override;
    function GetMethodGETId: String; override;
    function GetMethodGETWhere: String; override;
    function GetMethodPOST: String; override;
    function GetMethodPUT: String; override;
    function GetMethodDELETE: String; override;
    function GetMethodGETNextPacket: String; override;
    function GetMethodGETNextPacketWhere: String; override;
    function GetServerUse: Boolean; override;
    function Execute(const AResource, ASubResource: String;
      const ARequestMethod: TRESTRequestMethodType;
      const AParams: TProc = nil): String; overload; override;
    procedure SetClassNotServerUse(const Value: Boolean); override;
    procedure AddParam(const AValue: String); override;
    procedure AddQueryParam(const AValue: String); override;
    procedure AddBodyParam(const AValue: String); override;
  end;

implementation

{ TDriverRestWS }

procedure TRESTDriverWS.AddBodyParam(const AValue: String);
begin
  inherited;
  FConnection.AddBodyParam(AValue);
end;

procedure TRESTDriverWS.AddParam(const AValue: String);
begin
  inherited;
  FConnection.AddParam(AValue);
end;

procedure TRESTDriverWS.AddQueryParam(const AValue: String);
begin
  inherited;
  FConnection.AddQueryParam(AValue);
end;

constructor TRESTDriverWS.Create(AConnection: TComponent);
begin
  inherited;
  FConnection := AConnection as TRESTClientWS;
end;

destructor TRESTDriverWS.Destroy;
begin
  FConnection := nil;
  inherited;
end;

function TRESTDriverWS.Execute(const AResource, ASubResource: String;
  const ARequestMethod: TRESTRequestMethodType; const AParams: TProc): String;
begin
  Result := FConnection
              .Execute(AResource, ASubResource, ARequestMethod, AParams);
end;

function TRESTDriverWS.GetBaseURL: String;
begin
  Result := FConnection.BaseURL;
end;

function TRESTDriverWS.GetMethodDELETE: String;
begin
  Result := FConnection.MethodDelete;
end;

function TRESTDriverWS.GetMethodPOST: String;
begin
  Result := FConnection.MethodPOST;
end;

function TRESTDriverWS.GetMethodGETNextPacket: String;
begin
  Result := FConnection.MethodGETNextPacket;
end;

function TRESTDriverWS.GetMethodGETNextPacketWhere: String;
begin
  Result := FConnection.MethodGETNextPacketWhere;
end;

function TRESTDriverWS.GetMethodGET: String;
begin
  Result := FConnection.MethodGET;
end;

function TRESTDriverWS.GetMethodGETId: String;
begin
  Result := FConnection.MethodGETId;
end;

function TRESTDriverWS.GetMethodGETWhere: String;
begin
  Result := FConnection.MethodGETWhere;
end;

function TRESTDriverWS.GetMethodPUT: String;
begin
  Result := FConnection.MethodPUT;
end;

function TRESTDriverWS.GetServerUse: Boolean;
begin
  Result := FConnection.ORMBrServerUse;
end;

procedure TRESTDriverWS.SetClassNotServerUse(const Value: Boolean);
begin
  FConnection.SetClassNotServerUse(Value);
end;

end.

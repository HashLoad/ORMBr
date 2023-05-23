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

unit ormbr.client.restdriver.wirl;

interface

uses
  Classes,
  SysUtils,
  ormbr.client.wirl,
  ormbr.client.methods,
  ormbr.driver.rest;

type
  TRESTDriverWiRL = class(TRESTDriver)
  protected
    FConnection: TRESTClientWiRL;
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
    function GetUsername: String; override;
    function GetPassword: String; override;
    function GetMethodToken: String; override;
    function Execute(const AResource, ASubResource: String;
      const ARequestMethod: TRESTRequestMethodType;
      const AParams: TProc = nil): String; overload; override;
    procedure SetClassNotServerUse(const Value: Boolean); override;
    procedure AddParam(const AValue: String); override;
    procedure AddQueryParam(const AValue: String); override;
    procedure AddBodyParam(const AValue: String); override;
  end;

implementation

{ TDriverRestWiRL }

procedure TRESTDriverWiRL.AddBodyParam(const AValue: String);
begin
  inherited;
  FConnection.AddBodyParam(AValue);
end;

procedure TRESTDriverWiRL.AddParam(const AValue: String);
begin
  inherited;
  FConnection.AddParam(AValue);
end;

procedure TRESTDriverWiRL.AddQueryParam(const AValue: String);
begin
  inherited;
  FConnection.AddQueryParam(AValue);
end;

constructor TRESTDriverWiRL.Create(AConnection: TComponent);
begin
  inherited;
  FConnection := AConnection as TRESTClientWiRL;
end;

destructor TRESTDriverWiRL.Destroy;
begin
  FConnection := nil;
  inherited;
end;

function TRESTDriverWiRL.Execute(const AResource, ASubResource: String;
  const ARequestMethod: TRESTRequestMethodType; const AParams: TProc): String;
begin
  Result := FConnection
              .Execute(AResource, ASubResource, ARequestMethod, AParams);
end;

function TRESTDriverWiRL.GetBaseURL: String;
begin
  Result := FConnection.BaseURL;
end;

function TRESTDriverWiRL.GetMethodDELETE: String;
begin
  Result := FConnection.MethodDelete;
end;

function TRESTDriverWiRL.GetMethodPOST: String;
begin
  Result := FConnection.MethodPOST;
end;

function TRESTDriverWiRL.GetMethodGETNextPacket: String;
begin
  Result := FConnection.MethodGETNextPacket;
end;

function TRESTDriverWiRL.GetMethodGETNextPacketWhere: String;
begin
  Result := FConnection.MethodGETNextPacketWhere;
end;

function TRESTDriverWiRL.GetMethodGET: String;
begin
  Result := FConnection.MethodGET;
end;

function TRESTDriverWiRL.GetMethodGETId: String;
begin
  Result := FConnection.MethodGETId;
end;

function TRESTDriverWiRL.GetMethodGETWhere: String;
begin
  Result := FConnection.MethodGETWhere;
end;

function TRESTDriverWiRL.GetMethodPUT: String;
begin
  Result := FConnection.MethodPUT;
end;

function TRESTDriverWiRL.GetMethodToken: String;
begin

end;

function TRESTDriverWiRL.GetPassword: String;
begin

end;

function TRESTDriverWiRL.GetServerUse: Boolean;
begin
  Result := FConnection.ORMBrServerUse;
end;

function TRESTDriverWiRL.GetUsername: String;
begin

end;

procedure TRESTDriverWiRL.SetClassNotServerUse(const Value: Boolean);
begin
  FConnection.SetClassNotServerUse(Value);
end;

end.

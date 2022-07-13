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

unit ormbr.driver.rest.wirl;

interface

uses
  Classes,
  SysUtils,
  ormbr.client.wirl,
  ormbr.client.methods,
  ormbr.driver.rest;

type
  TDriverRestWiRL = class(TDriverRest)
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

procedure TDriverRestWiRL.AddBodyParam(const AValue: String);
begin
  inherited;
  FConnection.AddBodyParam(AValue);
end;

procedure TDriverRestWiRL.AddParam(const AValue: String);
begin
  inherited;
  FConnection.AddParam(AValue);
end;

procedure TDriverRestWiRL.AddQueryParam(const AValue: String);
begin
  inherited;
  FConnection.AddQueryParam(AValue);
end;

constructor TDriverRestWiRL.Create(AConnection: TComponent);
begin
  inherited;
  FConnection := AConnection as TRESTClientWiRL;
end;

destructor TDriverRestWiRL.Destroy;
begin
  FConnection := nil;
  inherited;
end;

function TDriverRestWiRL.Execute(const AResource, ASubResource: String;
  const ARequestMethod: TRESTRequestMethodType; const AParams: TProc): String;
begin
  Result := FConnection
              .Execute(AResource, ASubResource, ARequestMethod, AParams);
end;

function TDriverRestWiRL.GetBaseURL: String;
begin
  Result := FConnection.BaseURL;
end;

function TDriverRestWiRL.GetMethodDELETE: String;
begin
  Result := FConnection.MethodDelete;
end;

function TDriverRestWiRL.GetMethodPOST: String;
begin
  Result := FConnection.MethodPOST;
end;

function TDriverRestWiRL.GetMethodGETNextPacket: String;
begin
  Result := FConnection.MethodGETNextPacket;
end;

function TDriverRestWiRL.GetMethodGETNextPacketWhere: String;
begin
  Result := FConnection.MethodGETNextPacketWhere;
end;

function TDriverRestWiRL.GetMethodGET: String;
begin
  Result := FConnection.MethodGET;
end;

function TDriverRestWiRL.GetMethodGETId: String;
begin
  Result := FConnection.MethodGETId;
end;

function TDriverRestWiRL.GetMethodGETWhere: String;
begin
  Result := FConnection.MethodGETWhere;
end;

function TDriverRestWiRL.GetMethodPUT: String;
begin
  Result := FConnection.MethodPUT;
end;

function TDriverRestWiRL.GetMethodToken: String;
begin

end;

function TDriverRestWiRL.GetPassword: String;
begin

end;

function TDriverRestWiRL.GetServerUse: Boolean;
begin
  Result := FConnection.ORMBrServerUse;
end;

function TDriverRestWiRL.GetUsername: String;
begin

end;

procedure TDriverRestWiRL.SetClassNotServerUse(const Value: Boolean);
begin
  FConnection.SetClassNotServerUse(Value);
end;

end.

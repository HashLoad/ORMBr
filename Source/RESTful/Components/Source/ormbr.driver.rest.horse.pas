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

unit ormbr.driver.rest.horse;

interface

uses
  Classes,
  SysUtils,
  ormbr.client.horse,
  ormbr.driver.rest,
  ormbr.client.methods;

type
  // Classe de conexão concreta com dbExpress
  TDriverRestHorse = class(TDriverRest)
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

{ TDriverRestHorse }

procedure TDriverRestHorse.AddBodyParam(const AValue: String);
begin
  inherited;
  FConnection.AddBodyParam(AValue);
end;

procedure TDriverRestHorse.AddParam(const AValue: String);
begin
  inherited;
  FConnection.AddParam(AValue);
end;

procedure TDriverRestHorse.AddQueryParam(const AValue: String);
begin
  inherited;
  FConnection.AddQueryParam(AValue);
end;

constructor TDriverRestHorse.Create(AConnection: TComponent);
begin
  inherited;
  FConnection := AConnection as TRESTClientHorse;
end;

destructor TDriverRestHorse.Destroy;
begin
  FConnection := nil;
  inherited;
end;

function TDriverRestHorse.Execute(const AResource, ASubResource: String;
  const ARequestMethod: TRESTRequestMethodType; const AParams: TProc): String;
begin
  Result := FConnection
              .Execute(AResource, ASubResource, ARequestMethod, AParams);
end;

function TDriverRestHorse.GetBaseURL: String;
begin
  Result := FConnection.BaseURL;
end;

function TDriverRestHorse.GetFullURL: String;
begin
  Result := FConnection.FullURL;
end;

function TDriverRestHorse.GetMethodDELETE: String;
begin
  Result := FConnection.MethodDelete;
end;

function TDriverRestHorse.GetMethodPOST: String;
begin
  Result := FConnection.MethodPOST;
end;

function TDriverRestHorse.GetMethodGETNextPacket: String;
begin
  Result := FConnection.MethodGETNextPacket;
end;

function TDriverRestHorse.GetMethodGETNextPacketWhere: String;
begin
  Result := FConnection.MethodGETNextPacketWhere;
end;

function TDriverRestHorse.GetMethodGET: String;
begin
  Result := FConnection.MethodGET;
end;

function TDriverRestHorse.GetMethodGETId: String;
begin
  Result := FConnection.MethodGETId;
end;

function TDriverRestHorse.GetMethodGETWhere: String;
begin
  Result := FConnection.MethodGETWhere;
end;

function TDriverRestHorse.GetMethodPUT: String;
begin
  Result := FConnection.MethodPUT;
end;

function TDriverRestHorse.GetServerUse: Boolean;
begin
  Result := FConnection.ORMBrServerUse;
end;

procedure TDriverRestHorse.SetClassNotServerUse(const Value: Boolean);
begin
  FConnection.SetClassNotServerUse(Value);
end;

end.

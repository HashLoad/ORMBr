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

unit ormbr.client.restdriver.mars;

interface

uses
  Classes,
  SysUtils,
  ormbr.client.mars,
  ormbr.client.methods,
  ormbr.driver.rest;

type
  /// <summary>
  /// Classe de conexão concreta com MARS
  /// </summary>
  TRESTDriverMARS = class(TRESTDriver)
  protected
    FConnection: TRESTClientMARS;
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
      const ARequestMethod: TRESTRequestMethodType; const AParams: TProc = nil): String; overload; override;
    procedure SetClassNotServerUse(const Value: Boolean); override;
    procedure AddParam(const AValue: String); override;
    procedure AddQueryParam(const AValue: String); override;
    procedure AddBodyParam(const AValue: String); override;
  end;

implementation

{ TDriverRestMARS }

procedure TRESTDriverMARS.AddBodyParam(const AValue: String);
begin
  inherited;
  FConnection.AddBodyParam(AValue);
end;

procedure TRESTDriverMARS.AddParam(const AValue: String);
begin
  inherited;
  FConnection.AddParam(AValue);
end;

procedure TRESTDriverMARS.AddQueryParam(const AValue: String);
begin
  inherited;
  FConnection.AddQueryParam(AValue);
end;

constructor TRESTDriverMARS.Create(AConnection: TComponent);
begin
  inherited;
  FConnection := AConnection as TRESTClientMARS;
end;

destructor TRESTDriverMARS.Destroy;
begin
  FConnection := nil;
  inherited;
end;

function TRESTDriverMARS.Execute(const AResource, ASubResource: String;
  const ARequestMethod: TRESTRequestMethodType; const AParams: TProc): String;
begin
  Result := FConnection
              .Execute(AResource, ASubResource, ARequestMethod, AParams);
end;

function TRESTDriverMARS.GetBaseURL: String;
begin
  Result := FConnection.BaseURL;
end;

function TRESTDriverMARS.GetMethodDELETE: String;
begin
  Result := FConnection.MethodDelete;
end;

function TRESTDriverMARS.GetMethodPOST: String;
begin
  Result := FConnection.MethodPOST;
end;

function TRESTDriverMARS.GetMethodGETNextPacket: String;
begin
  Result := FConnection.MethodGETNextPacket;
end;

function TRESTDriverMARS.GetMethodGETNextPacketWhere: String;
begin
  Result := FConnection.MethodGETNextPacketWhere;
end;

function TRESTDriverMARS.GetMethodGET: String;
begin
  Result := FConnection.MethodGET;
end;

function TRESTDriverMARS.GetMethodGETId: String;
begin
  Result := FConnection.MethodGETID;
end;

function TRESTDriverMARS.GetMethodGETWhere: String;
begin
  Result := FConnection.MethodGETWhere;
end;

function TRESTDriverMARS.GetMethodPUT: String;
begin
  Result := FConnection.MethodPUT;
end;

function TRESTDriverMARS.GetServerUse: Boolean;
begin
  Result := FConnection.ORMBrServerUse;
end;

procedure TRESTDriverMARS.SetClassNotServerUse(const Value: Boolean);
begin
  FConnection.SetClassNotServerUse(Value);
end;

end.

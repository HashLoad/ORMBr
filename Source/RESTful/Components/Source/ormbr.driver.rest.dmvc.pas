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

unit ormbr.driver.rest.dmvc;

interface

uses
  Classes,
  SysUtils,
  ormbr.client.dmvc,
  ormbr.client.methods,
  ormbr.driver.rest;

type
  /// <summary>
  /// Classe de conexão concreta com Delphi MVC
  /// </summary>
  TDriverRestDMVC = class(TDriverRest)
  protected
    FConnection: TRESTClientDelphiMVC;
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

{ TDriverRestDMVC }

procedure TDriverRestDMVC.AddBodyParam(const AValue: String);
begin
  inherited;
  FConnection.AddBodyParam(AValue);
end;

procedure TDriverRestDMVC.AddParam(const AValue: String);
begin
  inherited;
  FConnection.AddParam(AValue);
end;

procedure TDriverRestDMVC.AddQueryParam(const AValue: String);
begin
  inherited;
  FConnection.AddQueryParam(AValue);
end;

constructor TDriverRestDMVC.Create(AConnection: TComponent);
begin
  inherited;
  FConnection := AConnection as TRESTClientDelphiMVC;
end;

destructor TDriverRestDMVC.Destroy;
begin
  FConnection := nil;
  inherited;
end;

function TDriverRestDMVC.Execute(const AResource, ASubResource: String;
  const ARequestMethod: TRESTRequestMethodType; const AParams: TProc): String;
begin
  Result := FConnection
              .Execute(AResource, ASubResource, ARequestMethod, AParams);
end;

function TDriverRestDMVC.GetBaseURL: String;
begin
  Result := FConnection.BaseURL;
end;

function TDriverRestDMVC.GetMethodDELETE: String;
begin
  Result := FConnection.MethodDelete;
end;

function TDriverRestDMVC.GetMethodPOST: String;
begin
  Result := FConnection.MethodPOST;
end;

function TDriverRestDMVC.GetMethodGETNextPacket: String;
begin
  Result := FConnection.MethodGETNextPacket;
end;

function TDriverRestDMVC.GetMethodGETNextPacketWhere: String;
begin
  Result := FConnection.MethodGETNextPacketWhere;
end;

function TDriverRestDMVC.GetMethodGET: String;
begin
  Result := FConnection.MethodGET;
end;

function TDriverRestDMVC.GetMethodGETId: String;
begin
  Result := FConnection.MethodGETId;
end;

function TDriverRestDMVC.GetMethodGETWhere: String;
begin
  Result := FConnection.MethodGETWhere;
end;

function TDriverRestDMVC.GetMethodPUT: String;
begin
  Result := FConnection.MethodPUT;
end;

function TDriverRestDMVC.GetServerUse: Boolean;
begin
  Result := FConnection.ORMBrServerUse;
end;

procedure TDriverRestDMVC.SetClassNotServerUse(const Value: Boolean);
begin
  FConnection.SetClassNotServerUse(Value);
end;

end.

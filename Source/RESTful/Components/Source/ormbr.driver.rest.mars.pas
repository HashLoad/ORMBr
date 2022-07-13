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

unit ormbr.driver.rest.mars;

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
  TDriverRestMARS = class(TDriverRest)
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

procedure TDriverRestMARS.AddBodyParam(const AValue: String);
begin
  inherited;
  FConnection.AddBodyParam(AValue);
end;

procedure TDriverRestMARS.AddParam(const AValue: String);
begin
  inherited;
  FConnection.AddParam(AValue);
end;

procedure TDriverRestMARS.AddQueryParam(const AValue: String);
begin
  inherited;
  FConnection.AddQueryParam(AValue);
end;

constructor TDriverRestMARS.Create(AConnection: TComponent);
begin
  inherited;
  FConnection := AConnection as TRESTClientMARS;
end;

destructor TDriverRestMARS.Destroy;
begin
  FConnection := nil;
  inherited;
end;

function TDriverRestMARS.Execute(const AResource, ASubResource: String;
  const ARequestMethod: TRESTRequestMethodType; const AParams: TProc): String;
begin
  Result := FConnection
              .Execute(AResource, ASubResource, ARequestMethod, AParams);
end;

function TDriverRestMARS.GetBaseURL: String;
begin
  Result := FConnection.BaseURL;
end;

function TDriverRestMARS.GetMethodDELETE: String;
begin
  Result := FConnection.MethodDelete;
end;

function TDriverRestMARS.GetMethodPOST: String;
begin
  Result := FConnection.MethodPOST;
end;

function TDriverRestMARS.GetMethodGETNextPacket: String;
begin
  Result := FConnection.MethodGETNextPacket;
end;

function TDriverRestMARS.GetMethodGETNextPacketWhere: String;
begin
  Result := FConnection.MethodGETNextPacketWhere;
end;

function TDriverRestMARS.GetMethodGET: String;
begin
  Result := FConnection.MethodGET;
end;

function TDriverRestMARS.GetMethodGETId: String;
begin
  Result := FConnection.MethodGETID;
end;

function TDriverRestMARS.GetMethodGETWhere: String;
begin
  Result := FConnection.MethodGETWhere;
end;

function TDriverRestMARS.GetMethodPUT: String;
begin
  Result := FConnection.MethodPUT;
end;

function TDriverRestMARS.GetServerUse: Boolean;
begin
  Result := FConnection.ORMBrServerUse;
end;

procedure TDriverRestMARS.SetClassNotServerUse(const Value: Boolean);
begin
  FConnection.SetClassNotServerUse(Value);
end;

end.

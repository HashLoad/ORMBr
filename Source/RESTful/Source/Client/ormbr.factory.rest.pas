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

unit ormbr.factory.rest;

interface

uses
  Classes,
  SysUtils,
  ormbr.driver.rest,
  ormbr.client.methods,
  ormbr.client.interfaces;

type
  // Fábrica de conexões abstratas
  TFactoryRestConnection = class abstract(TInterfacedObject, IRESTConnection)
  private
    function GetBaseURL: String;
    function GetFullURL: String;
    function GetPassword: String;
    function GetUsername: String;
    function GetMethodGET: String;
    function GetMethodGETId: String;
    function GetMethodGETWhere: String;
    function GetMethodPOST: String;
    function GetMethodPUT: String;
    function GetMethodDELETE: String;
    function GetMethodGETNextPacket: String;
    function GetMethodGETNextPacketWhere: String;
    function GetMethodToken: String;
    function GetServerUse: Boolean;
  protected
    FCommandMonitor: ICommandMonitor;
    FDriverConnection: TDriverRest;
  public
    constructor Create(AConnection: TComponent); virtual;
    destructor Destroy; override;
    procedure SetCommandMonitor(AMonitor: ICommandMonitor);
    procedure SetClassNotServerUse(const Value: Boolean);
    procedure AddParam(AValue: String); virtual;
    procedure AddBodyParam(AValue: String); virtual;
    procedure AddQueryParam(AValue: String); virtual;
    function CommandMonitor: ICommandMonitor;
    function Execute(const AResource, ASubResource: String;
      const ARequestMethod: TRESTRequestMethodType;
      const AParams: TProc = nil): String; overload; virtual; abstract;
    function Execute(const AResource: String; const ARequestMethod: TRESTRequestMethodType;
      const AParams: TProc = nil): String; overload; virtual; abstract;
    property BaseURL: String read GetBaseURL;
    property FullURL: String read GetFullURL;
    property Username: String read GetUsername;
    property Password: String read GetPassword;
    property MethodGET: String read GetMethodGET;
    property MethodGETId: String read GetMethodGETId;
    property MethodGETWhere: String read GetMethodGETWhere;
    property MethodPOST: String read GetMethodPOST;
    property MethodPUT: String read GetMethodPUT;
    property MethodDELETE: String read GetMethodDELETE;
    property MethodGETNextPacket: String read GetMethodGETNextPacket;
    property MethodGETNextPacketWhere: String read GetMethodGETNextPacketWhere;
    property MethodToken: String read GetMethodGETNextPacketWhere;
    property ServerUse: Boolean read GetServerUse;
  end;

implementation

{ TFactoryRestConnection }

constructor TFactoryRestConnection.Create(AConnection: TComponent);
begin

end;

destructor TFactoryRestConnection.Destroy;
begin
  if Assigned(FDriverConnection) then
    FDriverConnection.Free;
  inherited;
end;

procedure TFactoryRestConnection.AddBodyParam(AValue: String);
begin
  FDriverConnection.AddBodyParam(AValue);
end;

procedure TFactoryRestConnection.AddParam(AValue: String);
begin
  FDriverConnection.AddParam(AValue);
end;

procedure TFactoryRestConnection.AddQueryParam(AValue: String);
begin
  FDriverConnection.AddQueryParam(AValue);
end;

function TFactoryRestConnection.CommandMonitor: ICommandMonitor;
begin
  Result := FCommandMonitor;
end;

function TFactoryRestConnection.GetBaseURL: String;
begin
  Result := FDriverConnection.GetBaseURL;
end;

function TFactoryRestConnection.GetFullURL: String;
begin
  Result := FDriverConnection.GetFullURL;
end;

function TFactoryRestConnection.GetMethodDELETE: String;
begin
  Result := FDriverConnection.GetMethodDELETE;
end;

function TFactoryRestConnection.GetMethodPOST: String;
begin
  Result := FDriverConnection.GetMethodPOST;
end;

function TFactoryRestConnection.GetMethodGETNextPacket: String;
begin
  Result := FDriverConnection.GetMethodGETNextPacket;
end;

function TFactoryRestConnection.GetMethodGETNextPacketWhere: String;
begin
  Result := FDriverConnection.GetMethodGETNextPacketWhere;
end;

function TFactoryRestConnection.GetMethodGET: String;
begin
  Result := FDriverConnection.GetMethodGET;
end;

function TFactoryRestConnection.GetMethodGETId: String;
begin
  Result := FDriverConnection.GetMethodGETId;
end;

function TFactoryRestConnection.GetMethodGETWhere: String;
begin
  Result := FDriverConnection.GetMethodGETWhere;
end;

function TFactoryRestConnection.GetMethodToken: String;
begin
  Result := FDriverConnection.GetMethodToken;
end;

function TFactoryRestConnection.GetMethodPUT: String;
begin
  Result := FDriverConnection.GetMethodPUT;
end;

function TFactoryRestConnection.GetPassword: String;
begin
  Result := FDriverConnection.GetPassword;
end;

function TFactoryRestConnection.GetServerUse: Boolean;
begin
  Result := FDriverConnection.GetServerUse;
end;

function TFactoryRestConnection.GetUsername: String;
begin
  Result := FDriverConnection.GetUsername;
end;

procedure TFactoryRestConnection.SetClassNotServerUse(const Value: Boolean);
begin
  FDriverConnection.SetClassNotServerUse(Value);
end;

procedure TFactoryRestConnection.SetCommandMonitor(AMonitor: ICommandMonitor);
begin
  FCommandMonitor := AMonitor;
end;

end.

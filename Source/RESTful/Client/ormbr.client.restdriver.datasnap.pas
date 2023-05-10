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

unit ormbr.client.restdriver.datasnap;

interface

uses
  Classes,
  SysUtils,
  ormbr.client.datasnap,
  ormbr.client.restdriver,
  ormbr.client.methods;

type
  /// <summary>
  /// Classe de conexão concreta com dbExpress
  /// </summary>
  TRESTDriverDatasnap = class(TRESTDriver)
  protected
    FConnection: TRESTClientDataSnap;
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

{ TDriverRestDatasnap }

procedure TRESTDriverDatasnap.AddBodyParam(const AValue: String);
begin
  inherited;
  FConnection.AddBodyParam(AValue);
end;

procedure TRESTDriverDatasnap.AddParam(const AValue: String);
begin
  inherited;
  FConnection.AddParam(AValue);
end;

procedure TRESTDriverDatasnap.AddQueryParam(const AValue: String);
begin
  inherited;
  FConnection.AddQueryParam(AValue);
end;

constructor TRESTDriverDatasnap.Create(AConnection: TComponent);
begin
  inherited;
  FConnection := AConnection as TRESTClientDataSnap;
end;

destructor TRESTDriverDatasnap.Destroy;
begin
  FConnection := nil;
  inherited;
end;

function TRESTDriverDatasnap.Execute(const AResource, ASubResource: String;
  const ARequestMethod: TRESTRequestMethodType; const AParams: TProc): String;
begin
  Result := FConnection
              .Execute(AResource, ASubResource, ARequestMethod, AParams);
end;

function TRESTDriverDatasnap.GetBaseURL: String;
begin
  Result := FConnection.BaseURL;
end;

function TRESTDriverDatasnap.GetMethodDELETE: String;
begin
  Result := FConnection.MethodDelete;
end;

function TRESTDriverDatasnap.GetMethodPOST: String;
begin
  Result := FConnection.MethodPOST;
end;

function TRESTDriverDatasnap.GetMethodGETNextPacket: String;
begin
  Result := FConnection.MethodGETNextPacket;
end;

function TRESTDriverDatasnap.GetMethodGETNextPacketWhere: String;
begin
  Result := FConnection.MethodGETNextPacketWhere;
end;

function TRESTDriverDatasnap.GetMethodGET: String;
begin
  Result := FConnection.MethodGET;
end;

function TRESTDriverDatasnap.GetMethodGETId: String;
begin
  Result := FConnection.MethodGETId;
end;

function TRESTDriverDatasnap.GetMethodGETWhere: String;
begin
  Result := FConnection.MethodGETWhere;
end;

function TRESTDriverDatasnap.GetMethodPUT: String;
begin
  Result := FConnection.MethodPUT;
end;

function TRESTDriverDatasnap.GetServerUse: Boolean;
begin
  Result := FConnection.ORMBrServerUse;
end;

procedure TRESTDriverDatasnap.SetClassNotServerUse(const Value: Boolean);
begin
  FConnection.SetClassNotServerUse(Value);
end;

end.

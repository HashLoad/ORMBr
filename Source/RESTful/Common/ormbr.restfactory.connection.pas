{
      ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Versão 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos é permitido copiar e distribuir cópias deste documento de
       licença, mas mudá-lo não é permitido.

       Esta versão da GNU Lesser General Public License incorpora
       os termos e condições da versão 3 da GNU General Public License
       Licença, complementado pelas permissões adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)
}

unit ormbr.restfactory.connection;

interface

uses
  Classes,
  SysUtils,
  ormbr.client.restdriver,
  ormbr.client.methods,
  ormbr.restfactory.interfaces;

type
  // Fábrica de conexões abstratas
  TRESTFactoryConnection = class abstract(TInterfacedObject, IRESTConnection)
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
    FDriverConnection: TRESTDriver;
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

constructor TRESTFactoryConnection.Create(AConnection: TComponent);
begin

end;

destructor TRESTFactoryConnection.Destroy;
begin
  if Assigned(FDriverConnection) then
    FDriverConnection.Free;
  inherited;
end;

procedure TRESTFactoryConnection.AddBodyParam(AValue: String);
begin
  FDriverConnection.AddBodyParam(AValue);
end;

procedure TRESTFactoryConnection.AddParam(AValue: String);
begin
  FDriverConnection.AddParam(AValue);
end;

procedure TRESTFactoryConnection.AddQueryParam(AValue: String);
begin
  FDriverConnection.AddQueryParam(AValue);
end;

function TRESTFactoryConnection.CommandMonitor: ICommandMonitor;
begin
  Result := FCommandMonitor;
end;

function TRESTFactoryConnection.GetBaseURL: String;
begin
  Result := FDriverConnection.GetBaseURL;
end;

function TRESTFactoryConnection.GetFullURL: String;
begin
  Result := FDriverConnection.GetFullURL;
end;

function TRESTFactoryConnection.GetMethodDELETE: String;
begin
  Result := FDriverConnection.GetMethodDELETE;
end;

function TRESTFactoryConnection.GetMethodPOST: String;
begin
  Result := FDriverConnection.GetMethodPOST;
end;

function TRESTFactoryConnection.GetMethodGETNextPacket: String;
begin
  Result := FDriverConnection.GetMethodGETNextPacket;
end;

function TRESTFactoryConnection.GetMethodGETNextPacketWhere: String;
begin
  Result := FDriverConnection.GetMethodGETNextPacketWhere;
end;

function TRESTFactoryConnection.GetMethodGET: String;
begin
  Result := FDriverConnection.GetMethodGET;
end;

function TRESTFactoryConnection.GetMethodGETId: String;
begin
  Result := FDriverConnection.GetMethodGETId;
end;

function TRESTFactoryConnection.GetMethodGETWhere: String;
begin
  Result := FDriverConnection.GetMethodGETWhere;
end;

function TRESTFactoryConnection.GetMethodToken: String;
begin
  Result := FDriverConnection.GetMethodToken;
end;

function TRESTFactoryConnection.GetMethodPUT: String;
begin
  Result := FDriverConnection.GetMethodPUT;
end;

function TRESTFactoryConnection.GetPassword: String;
begin
  Result := FDriverConnection.GetPassword;
end;

function TRESTFactoryConnection.GetServerUse: Boolean;
begin
  Result := FDriverConnection.GetServerUse;
end;

function TRESTFactoryConnection.GetUsername: String;
begin
  Result := FDriverConnection.GetUsername;
end;

procedure TRESTFactoryConnection.SetClassNotServerUse(const Value: Boolean);
begin
  FDriverConnection.SetClassNotServerUse(Value);
end;

procedure TRESTFactoryConnection.SetCommandMonitor(AMonitor: ICommandMonitor);
begin
  FCommandMonitor := AMonitor;
end;

end.

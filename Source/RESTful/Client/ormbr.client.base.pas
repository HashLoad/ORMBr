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

unit ormbr.client.base;

interface

uses
  Classes,
  SysUtils,
  ormbr.restcomponent,
  ormbr.client.consts,
  ormbr.restfactory.interfaces;

type
  TAuthenticatorType = (atNoAuth, atBasicAuth, atBearerToken,
                        atOAuth1, atOAuth2);

  TAuthenticator = class(TPersistent)
  private
    FUsername: String;
    FPassword: String;
    FToken: String;
    FAuthenticatorType: TAuthenticatorType;
    function GetUsername: String;
    procedure SetUsername(const Value: String);
    function GetPassword: String;
    procedure SetPassword(const Value: String);
    function GetAuthenticatorType: TAuthenticatorType;
    procedure SetAuthenticatorType(const Value: TAuthenticatorType);
  public
    constructor Create;
    destructor Destroy; override;
    property Token: String read FToken write FToken;
  published
    property Username: String read GetUsername write SetUsername;
    property Password: String read GetPassword write SetPassword;
    property AuthenticatorType: TAuthenticatorType read GetAuthenticatorType write SetAuthenticatorType;
  end;

  TRestProxyInfo = class(TPersistent)
  private
    FBasicByDefault: boolean;
    FProxyPort: Integer;
    FPassword: string;
    FUsername: string;
    FProxyServer: string;
  protected
    procedure AssignTo(ADestination: TPersistent); override;
  published
    property BasicAuthentication: boolean read FBasicByDefault write FBasicByDefault;
    property ProxyPassword: string read FPassword write FPassword;
    property ProxyPort: Integer read FProxyPort write FProxyPort;
    property ProxyServer: string read FProxyServer write FProxyServer;
    property ProxyUsername: string read FUsername write FUserName;
  end;

  TORMBrClientBase = class(TORMBrComponent)
  private
    procedure SetProxyParams(const Value: TRestProxyInfo);
    procedure SetAuthenticator(const Value: TAuthenticator);
  protected
    FRESTFactory: IRESTConnection;
    FProxyParams: TRestProxyInfo;
    FAuthenticator: TAuthenticator;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AsConnection: IRESTConnection; deprecated 'use SetCommandMonitor(AMonitor: ICommandMonitor)';
    procedure SetCommandMonitor(AMonitor: ICommandMonitor);
  published
    // Proxy Settings to be used by the client
    property ProxyParams: TRestProxyInfo read FProxyParams write SetProxyParams;
    property Authenticator: TAuthenticator read FAuthenticator write SetAuthenticator;
  end;

implementation

{ TORMBrClientBase }

function TORMBrClientBase.AsConnection: IRESTConnection;
begin
  Result := FRESTFactory;
end;

constructor TORMBrClientBase.Create(AOwner: TComponent);
begin
  inherited;
  FProxyParams := TRestProxyInfo.Create;
  FAuthenticator := TAuthenticator.Create;
end;

destructor TORMBrClientBase.Destroy;
begin
  FProxyParams.Free;
  FAuthenticator.Free;
  inherited;
end;

procedure TORMBrClientBase.SetAuthenticator(const Value: TAuthenticator);
begin
  FAuthenticator := Value;
end;

procedure TORMBrClientBase.SetCommandMonitor(AMonitor: ICommandMonitor);
begin
  FRESTFactory.SetCommandMonitor(AMonitor);
end;

procedure TORMBrClientBase.SetProxyParams(const Value: TRestProxyInfo);
begin
  FProxyParams := Value;
end;

{ TRestProxyInfo }

procedure TRestProxyInfo.AssignTo(ADestination: TPersistent);
var
  LDest: TRestProxyInfo;
begin
  if ADestination is TRestProxyInfo then
  begin
    LDest := TRestProxyInfo(ADestination);
    LDest.FPassword := FPassword;
    LDest.FProxyPort := FProxyPort;
    LDest.FProxyServer := FProxyServer;
    LDest.FUsername := FUsername;
    LDest.FBasicByDefault := FBasicByDefault;
  end
  else
  begin
    inherited AssignTo(ADestination);
  end;
end;

{ TAuthenticator }

constructor TAuthenticator.Create;
begin
  FUsername := '';
  FPassword := '';
  FToken    := '';
end;

destructor TAuthenticator.Destroy;
begin

  inherited;
end;

function TAuthenticator.GetAuthenticatorType: TAuthenticatorType;
begin
  Result := FAuthenticatorType;
end;

function TAuthenticator.GetPassword: String;
begin
  Result := FPassword;
end;

function TAuthenticator.GetUsername: String;
begin
  Result := FUsername;
end;

procedure TAuthenticator.SetAuthenticatorType(const Value: TAuthenticatorType);
begin
  FAuthenticatorType := Value;
end;

procedure TAuthenticator.SetPassword(const Value: String);
begin
  FPassword := Value;
end;

procedure TAuthenticator.SetUsername(const Value: String);
begin
  FUsername := Value;
end;

end.

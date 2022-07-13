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

unit ormbr.client.dmvc;

interface

uses
  DB,
  SysUtils,
  StrUtils,
  Classes,
  Generics.Collections,
  ormbr.rest.classes,
  ormbr.client,
  ormbr.client.base,
  ormbr.client.methods,

  MVCFramework.RESTClient;

type
  TRESTClientDelphiMVC = class(TORMBrClient)
  private
    FRESTClient: TRESTClient;
    FRESTResponse: IRESTResponse;
    procedure SetProxyParamsClientValues;
    procedure SetAuthenticatorTypeValues;
    procedure SetParamsBodyValue;
    procedure SetParamValues(AParams: PClientParam);
    function DoGET(const AURL, AResource, ASubResource: String;
      const AParams: array of string): String;
    function DoPOST(const AURL, AResource, ASubResource: String;
      const AParams: array of string): String;
    function DoPUT(const AURL, AResource, ASubResource: String;
      const AParams: array of string): String;
    function DoDELETE(const AURL, AResource, ASubResource: String;
      const AParams: array of string): String;
    function RemoveContextServerUse(const Value: String): string;
  protected
    procedure DoAfterCommand; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(const AResource, ASubResource: String;
      const ARequestMethod: TRESTRequestMethodType;
      const AParamsProc: TProc = nil): String; overload;
    function Execute(const AURL: String;
      const ARequestMethod: TRESTRequestMethodType;
      const AParamsProc: TProc = nil): String; overload;
  published
    property APIContext;
    property ORMBrServerUse;
  end;

implementation

uses
  ormbr.factory.rest.dmvc;

{$R 'RESTClientDelphiMVC.res'}

{ TRESTClientDelphiMVC }

constructor TRESTClientDelphiMVC.Create(AOwner: TComponent);
begin
  inherited;
  FRESTFactory := TFactoryRestDMVC.Create(Self);
  // Monta a URL base
  SetBaseURL;
end;

destructor TRESTClientDelphiMVC.Destroy;
begin
  if Assigned(FRESTClient) then
    FRESTClient.Free;
  inherited;
end;

procedure TRESTClientDelphiMVC.DoAfterCommand;
begin
  FStatusCode := FRESTResponse.ResponseCode;
  inherited;
end;

function TRESTClientDelphiMVC.DoDELETE(const AURL, AResource,
  ASubResource: String; const AParams: array of string): String;
begin
  FRequestMethod := 'DELETE';
  try
    FRESTResponse := FRESTClient.doDELETE(AURL, AParams);
    Result := FRESTResponse.BodyAsString;
    if FRESTResponse.HasError then
      raise Exception.Create(FRESTResponse.Error.ExceptionMessage);
  except
    on E: Exception do
    begin
      if Assigned(FErrorCommand) then
        FErrorCommand(GetBaseURL,
                      AResource,
                      ASubResource,
                      FRequestMethod,
                      E.Message,
                      FRESTResponse.ResponseCode)
      else
        raise ERESTConnectionError.Create(GetBaseURL,
                                          AResource,
                                          ASubResource,
                                          FRequestMethod,
                                          E.Message,
                                          FRESTResponse.ResponseCode);
    end;
  end;
end;

function TRESTClientDelphiMVC.DoGET(const AURL, AResource, ASubResource: String;
      const AParams: array of string): String;
begin
  FRequestMethod := 'GET';
  try
    FRESTResponse := FRESTClient.doGET(AURL, AParams);
    Result := FRESTResponse.BodyAsString;
    if FRESTResponse.HasError then
      raise Exception.Create(FRESTResponse.Error.ExceptionMessage);
  except
    on E: Exception do
    begin
      if Assigned(FErrorCommand) then
        FErrorCommand(GetBaseURL,
                      AResource,
                      ASubResource,
                      FRequestMethod,
                      E.Message,
                      FRESTResponse.ResponseCode)
      else
        raise ERESTConnectionError
                .Create(GetBaseURL,
                        AResource,
                        ASubResource,
                        FRequestMethod,
                        E.Message,
                        FRESTResponse.ResponseCode);
    end;
  end;
end;

function TRESTClientDelphiMVC.DoPOST(const AURL, AResource,
  ASubResource: String; const AParams: array of string): String;
begin
  FRequestMethod := 'POST';
  // Define valores dos parâmetros
  SetParamsBodyValue;
  // POST
  try
    FRESTResponse := FRESTClient.doPOST(AURL, AParams, FRESTClient.BodyParams.Text);
    Result := FRESTResponse.BodyAsString;
    if FRESTResponse.HasError then
      raise Exception.Create(FRESTResponse.Error.ExceptionMessage);
  except
    on E: Exception do
    begin
      if Assigned(FErrorCommand) then
        FErrorCommand(GetBaseURL,
                      AResource,
                      ASubResource,
                      FRequestMethod,
                      E.Message,
                      FRESTResponse.ResponseCode)
      else
        raise ERESTConnectionError
                .Create(GetBaseURL,
                        AResource,
                        ASubResource,
                        FRequestMethod,
                        E.Message,
                        FRESTResponse.ResponseCode);
    end;
  end;
end;

function TRESTClientDelphiMVC.DoPUT(const AURL, AResource, ASubResource: String;
  const AParams: array of string): String;
begin
  FRequestMethod := 'PUT';
  // Define valores dos parâmetros
  SetParamsBodyValue;
  // PUT
  try
    FRESTResponse := FRESTClient.doPUT(AURL, AParams, FRESTClient.BodyParams.Text);
    Result := FRESTResponse.BodyAsString;
    if FRESTResponse.HasError then
      raise Exception.Create(FRESTResponse.Error.ExceptionMessage);
  except
    on E: Exception do
    begin
      if Assigned(FErrorCommand) then
        FErrorCommand(GetBaseURL,
                      AResource,
                      ASubResource,
                      FRequestMethod,
                      E.Message,
                      FRESTResponse.ResponseCode)
      else
        raise ERESTConnectionError
                .Create(GetBaseURL,
                        AResource,
                        ASubResource,
                        FRequestMethod,
                        E.Message,
                        FRESTResponse.ResponseCode);
    end;
  end;
end;

function TRESTClientDelphiMVC.Execute(const AURL: String;
  const ARequestMethod: TRESTRequestMethodType;
  const AParamsProc: TProc): String;
var
  LURL: String;
  LParams: TClientParam;
begin
  Result := '';
  // Passa os dados de acesso para o RESTClient do Delphi MVC
  if not Assigned(FRESTClient) then
    FRESTClient := TRESTClient.Create(FHost, FPort);
  // Define valores de autenticação
  SetAuthenticatorTypeValues;
  // Executa a procedure de adição dos parâmetros
  if Assigned(AParamsProc) then
    AParamsProc();
  // Define dados do proxy
  SetProxyParamsClientValues;
  // Define valores dos parâmetros
  SetParamValues(@LParams);
  try
    // DoBeforeCommand
    DoBeforeCommand;

    case ARequestMethod of
      TRESTRequestMethodType.rtPOST:
        begin
          Result := DoPOST(AURL, '', '', LParams);
        end;
      TRESTRequestMethodType.rtPUT:
        begin
          Result := DoPUT(AURL, '', '', LParams);
        end;
      TRESTRequestMethodType.rtGET:
        begin
          Result := DoGET(AURL, '', '', LParams);
        end;
      TRESTRequestMethodType.rtDELETE:
        begin
          Result := DoDELETE(AURL, '', '', LParams);
        end;
      TRESTRequestMethodType.rtPATCH: ;
    end;
    // Passao JSON para VAR que poderá ser manipulada no evento AfterCommand
    FResponseString := Result;
    // DoAfterCommand
    DoAfterCommand;
    // Pega de volta JSON manipulado ou não no evento AfterCommand
    Result := FResponseString;
  finally
    FResponseString := '';
    FParams.Clear;
    FQueryParams.Clear;
    FBodyParams.Clear;
    FRESTClient.ClearHeaders;
  end;
end;

function TRESTClientDelphiMVC.Execute(const AResource, ASubResource: String;
  const ARequestMethod: TRESTRequestMethodType;
  const AParamsProc: TProc): String;
var
  LURL: String;
  LParams: TClientParam;

  procedure SetURLValue;
  var
    LResource: String;
    LSubResource: String;
  begin
    // Trata URL Base caso componente esteja usando servidor, mas a classe não.
    if (FServerUse) and (not FClassNotServerUse) then
      LResource := FAPIContext;
    // Nome do recurso
    LResource := LResource + '/' + AResource;
    // Nome do sub-recurso
    if Length(ASubResource) > 0 then
      LSubResource := '/' + ASubResource;
    /// URL completa
    LURL := LResource + LSubResource;
  end;

begin
  Result := '';
  // Passa os dados de acesso para o RESTClient do Delphi MVC
  if not Assigned(FRESTClient) then
    FRESTClient := TRESTClient.Create(FHost, FPort);
  // Define valores de autenticação
  SetAuthenticatorTypeValues;
  // Executa a procedure de adição dos parâmetros
  if Assigned(AParamsProc) then
    AParamsProc();
  // Define valor da URL
  SetURLValue;
  // Define dados do proxy
  SetProxyParamsClientValues;
  // Define valores dos parâmetros
  SetParamValues(@LParams);
  try
    // DoBeforeCommand
    DoBeforeCommand;

    case ARequestMethod of
      TRESTRequestMethodType.rtPOST:
        begin
          Result := DoPOST(LURL, AResource, ASubResource, LParams);
        end;
      TRESTRequestMethodType.rtPUT:
        begin
          Result := DoPUT(LURL, AResource, ASubResource, LParams);
        end;
      TRESTRequestMethodType.rtGET:
        begin
          Result := DoGET(LURL, AResource, ASubResource, LParams);
        end;
      TRESTRequestMethodType.rtDELETE:
        begin
          Result := DoDELETE(LURL, AResource, ASubResource, LParams);
        end;
      TRESTRequestMethodType.rtPATCH: ;
    end;
    // Passao JSON para VAR que poderá ser manipulada no evento AfterCommand
    FResponseString := Result;
    // DoAfterCommand
    DoAfterCommand;
    // Pega de volta JSON manipulado ou não no evento AfterCommand
    Result := FResponseString;
  finally
    FResponseString := '';
    FParams.Clear;
    FQueryParams.Clear;
    FBodyParams.Clear;
    FRESTClient.ClearHeaders;
  end;
end;

procedure TRESTClientDelphiMVC.SetAuthenticatorTypeValues;
var
  LAuthorized: Boolean;
begin
  LAuthorized := False;
  /// <summary> Dispara evento OnAuthentication </summary>
//  if Assigned(FAuthentication) and (not FPerformingAuthentication) then
//  begin
//    FPerformingAuthentication := True;
//    try
//      FAuthentication(LAuthorized);
//      if not LAuthorized then
//        raise Exception.Create('Unauthorized Authentication');
//    finally
//      FPerformingAuthentication := False;
//    end
//  end;
  case FAuthenticator.AuthenticatorType of
    atNoAuth:;
    atBasicAuth:
      begin
        FRESTClient.UseBasicAuthentication := True;
      end;
    atBearerToken,
    atOAuth1,
    atOAuth2:
      begin
        FRESTClient.UseBasicAuthentication := False;
        if Length(FAuthenticator.Token) > 0 then
        begin
          FRESTClient.Header('Authentication', 'Bearer ' + FAuthenticator.Token);
          Exit;
        end;
      end;
  end;
  FRESTClient.UserName := FAuthenticator.Username;
  FRESTClient.Password := FAuthenticator.Password;
//  FRESTClient.Header('username', FAuthenticator.Username);
//  FRESTClient.Header('password', FAuthenticator.Password);
end;

function TRESTClientDelphiMVC.RemoveContextServerUse(const Value: String): string;
begin
  Result := '';
end;

procedure TRESTClientDelphiMVC.SetParamsBodyValue;
var
  LFor: Integer;
begin
  // Passa os valores do BodyParams externo para o string
  for LFor := 0 to FBodyParams.Count -1 do
    FRESTClient.BodyParams.Add(FBodyParams.Items[LFor].AsString);
end;

procedure TRESTClientDelphiMVC.SetParamValues(AParams: PClientParam);
var
  LFor: Integer;
begin
  // Define o parametro do tipo array necessário para o Delphi MVC
  if FParams.Count > 0 then
  begin
    SetLength(AParams^, FParams.Count);
    // Passa os valores do Params externo para o array
    for LFor := 0 to FParams.Count -1 do
      AParams^[LFor] := FParams.Items[LFor].AsString;
  end;
  // Passa os valores do Query Params externo para o array
  for LFor := 0 to FQueryParams.Count -1 do
    FRESTClient.QueryStringParams.Add(FQueryParams.Items[LFor].AsString);
end;

procedure TRESTClientDelphiMVC.SetProxyParamsClientValues;
begin
  FRESTClient.ProxyServer := FProxyParams.ProxyServer;
  FRESTClient.ProxyPort := FProxyParams.ProxyPort;
  FRESTClient.Username := FProxyParams.ProxyUsername;
  FRESTClient.Password := FProxyParams.ProxyPassword;
end;

end.

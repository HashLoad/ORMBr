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

unit ormbr.client.mars;

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

  MARS.Client.CustomResource,
  MARS.Client.Resource,
  MARS.Client.Resource.JSON,
  MARS.Client.Application,
  MARS.Client.Client,
  MARS.Client.Client.Indy,
  MARS.Core.Utils,
  MARS.Client.Token;

type
  TRESTClientMARS = class(TORMBrClient)
  private
    FRESTClient: TMARSClient;
    FRESTClientApp: TMARSClientApplication;
    FRESTResource: TMARSClientResourceJSON;
    FRESTToken: TMARSClientToken;
    procedure SetProxyParamsClientValue;
    procedure SetParamsBodyValue(var AParams: String);
    procedure SetAuthenticatorTypeValues;
    procedure SetParamValues;
    function DoGET(const AResource, ASubResource: String): String;
    function DoPOST(const AResource, ASubResource: String): String;
    function DoPUT(const AResource, ASubResource: String): String;
    function DoDELETE(const AResource, ASubResource: String): String;
    function RemoveContextServerUse(const Value: String): string;
  protected
    procedure DoAfterCommand; override;
    procedure SetBaseURL; override;
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
    property RESTContext;
    property MethodToken;
    property ORMBrServerUse;
  end;

implementation

uses
  ormbr.factory.rest.mars;

{$R 'RESTClientMARS.res'}

{ TRESTClientMARS }

constructor TRESTClientMARS.Create(AOwner: TComponent);
begin
  inherited;
  FRESTFactory := TFactoryRestMARS.Create(Self);
  FRESTClient := TMARSClient.Create(Self);
  FRESTClientApp := TMARSClientApplication.Create(Self);
  FRESTClientApp.Client := FRESTClient;
  FRESTResource := TMARSClientResourceJSON.Create(Self);
  FRESTResource.Application := FRESTClientApp;
  FRESTToken := TMARSClientToken.Create(Self);
  FRESTToken.Application := FRESTClientApp;
  FAPIContext := 'default';
  FRESTContext := 'rest';
  // Monta a URL base
  SetBaseURL;
end;

destructor TRESTClientMARS.Destroy;
begin
  FRESTResource.Free;
  FRESTClientApp.Free;
  FRESTClient.Free;
  inherited;
end;

procedure TRESTClientMARS.DoAfterCommand;
begin
  FStatusCode := FRESTClient.ResponseStatusCode;
  inherited;
end;

function TRESTClientMARS.DoDELETE(const AResource, ASubResource: String): String;
begin
  FRequestMethod := 'DELETE';
  // Define valores dos parâmetros
  SetParamValues;
  // DELETE
  FRESTResource.DELETE(nil,
                       procedure(AResponse: TStream)
                       begin
                         AResponse.Position := 0;
                         FResponseString := StreamToString(AResponse);
                       end,
                       procedure(E: Exception)
                       begin
                         if Assigned(FErrorCommand) then
                           FErrorCommand(GetBaseURL,
                                         AResource,
                                         ASubResource,
                                         FRequestMethod,
                                         E.Message,
                                         FRESTClient.ResponseStatusCode)
                         else
                           raise ERESTConnectionError
                                   .Create(FRESTClient.MARSEngineURL,
                                           AResource,
                                           ASubResource,
                                           FRequestMethod,
                                           E.Message,
                                           FRESTClient.ResponseStatusCode);
                       end);
  Result := FResponseString;
end;

function TRESTClientMARS.DoGET(const AResource, ASubResource: String): String;
begin
  FRequestMethod := 'GET';
  // Define valores dos parâmetros
  SetParamValues;
  // GET
  Result := FRESTResource.GETAsString(nil, nil,
                                      procedure(E: Exception)
                                      begin
                                        if Assigned(FErrorCommand) then
                                          FErrorCommand(GetBaseURL,
                                                        AResource,
                                                        ASubResource,
                                                        FRequestMethod,
                                                        E.Message,
                                                        FRESTClient.ResponseStatusCode)
                                        else
                                          raise ERESTConnectionError
                                                  .Create(FRESTClient.MARSEngineURL,
                                                          AResource,
                                                          ASubResource,
                                                          FRequestMethod,
                                                          E.Message,
                                                          FRESTClient.ResponseStatusCode);
                                      end);
end;

function TRESTClientMARS.DoPOST(const AResource, ASubResource: String): String;
var
  LParams: String;
begin
  FRequestMethod := 'POST';
  // Define valores dos parâmetros
  SetParamsBodyValue(LParams);
  // POST
  FRESTResource.POST(procedure(AContent: TMemoryStream)
                     var
                       LWriter: TStreamWriter;
                     begin
                       LWriter := TStreamWriter.Create(AContent);
                       try
                         LWriter.Write(LParams);
                         AContent.Position := 0;
                       finally
                         LWriter.Free;
                       end;
                     end,
                     procedure(AResponse: TStream)
                     begin
                       AResponse.Position := 0;
                       FResponseString := StreamToString(AResponse);
                     end,
                     procedure(E: Exception)
                     begin
                       if Assigned(FErrorCommand) then
                         FErrorCommand(GetBaseURL,
                                       AResource,
                                       ASubResource,
                                       FRequestMethod,
                                       E.Message,
                                       FRESTClient.ResponseStatusCode)
                       else
                         raise ERESTConnectionError
                                 .Create(FRESTClient.MARSEngineURL,
                                         AResource,
                                         ASubResource,
                                         E.Message,
                                         FRequestMethod,
                                         FRESTClient.ResponseStatusCode);
                     end );
  Result := FResponseString;
end;

function TRESTClientMARS.DoPUT(const AResource, ASubResource: String): String;
var
  LParams: String;
begin
  FRequestMethod := 'PUT';
  // Define valores dos parâmetros
  SetParamsBodyValue(LParams);
  // PUT
  FRESTResource.PUT(procedure(AContent: TMemoryStream)
                    var
                      LWriter: TStreamWriter;
                    begin
                      LWriter := TStreamWriter.Create(AContent);
                      try
                        LWriter.Write(LParams);
                        AContent.Position := 0;
                      finally
                        LWriter.Free;
                      end;
                    end,
                    procedure(AResponse: TStream)
                    begin
                      AResponse.Position := 0;
                      FResponseString := StreamToString(AResponse);
                    end,
                    procedure(E: Exception)
                    begin
                      if Assigned(FErrorCommand) then
                        FErrorCommand(GetBaseURL,
                                      AResource,
                                      ASubResource,
                                      FRequestMethod,
                                      E.Message,
                                      FRESTClient.ResponseStatusCode)
                      else
                        raise ERESTConnectionError
                                .Create(FRESTClient.MARSEngineURL,
                                        AResource,
                                        ASubResource,
                                        FRequestMethod,
                                        E.Message,
                                        FRESTClient.ResponseStatusCode);
                    end );
  Result := FResponseString;
end;

function TRESTClientMARS.Execute(const AURL: String;
  const ARequestMethod: TRESTRequestMethodType;
  const AParamsProc: TProc): String;
var
  LFor: Integer;

  procedure SetURLValue;
  begin
    FRESTClient.MARSEngineURL := AURL;
    FRESTClientApp.AppName := '';
    FRESTResource.Resource := '';
    FRESTResource.QueryParams.Clear;
  end;

begin
  Result := '';
  // Executa a procedure de adição dos parâmetros
  if Assigned(AParamsProc) then
    AParamsProc();
  // Define valor da URL
  SetURLValue;
  // Define dados do proxy
  SetProxyParamsClientValue;
  // Define valores de autenticação
  SetAuthenticatorTypeValues;
  try
    // DoBeforeCommand
    DoBeforeCommand;

    case ARequestMethod of
      TRESTRequestMethodType.rtPOST:
        begin
          DoPOST('', '');
        end;
      TRESTRequestMethodType.rtPUT:
        begin
          DoPUT('', '');
        end;
      TRESTRequestMethodType.rtGET:
        begin
          DoGET('', '');
        end;
      TRESTRequestMethodType.rtDELETE:
        begin
          DoDELETE('', '');
        end;
      TRESTRequestMethodType.rtPATCH: ;
    end;
    // Passao JSON para a VAR que poderá ser manipulada no evento AfterCommand
    FResponseString := Result;
    // DoAfterCommand
    DoAfterCommand;
    // Pega de volta o JSON manipulado ou não no evento AfterCommand
    Result := FResponseString;
  finally
    FResponseString := '';
    FParams.Clear;
    FQueryParams.Clear;
    FBodyParams.Clear;
  end;
end;

function TRESTClientMARS.Execute(const AResource, ASubResource: String;
  const ARequestMethod: TRESTRequestMethodType;
  const AParamsProc: TProc): String;
var
  LFor: Integer;

  procedure SetURLValue;
  begin
    FRESTClientApp.AppName := FAPIContext;
    // Trata a URL Base caso o componente esteja para usar o servidor,
    // mas a classe não.
    if (FServerUse) and (FClassNotServerUse) then
      FRESTClientApp.AppName := RemoveContextServerUse(FRESTClientApp.AppName);

    FRESTClient.MARSEngineURL := GetBaseURL;
    FRESTResource.Resource := AResource;
    if Length(ASubResource) > 0 then
      FRESTResource.Resource := FRESTResource.Resource + '/' + ASubResource;
    FRESTResource.QueryParams.Clear;
  end;

begin
  Result := '';
  // Executa a procedure de adição dos parâmetros
  if Assigned(AParamsProc) then
    AParamsProc();
  // Define valor da URL
  SetURLValue;
  // Define dados do proxy
  SetProxyParamsClientValue;
  // Define valores de autenticação
  SetAuthenticatorTypeValues;
  try
    // DoBeforeCommand
    DoBeforeCommand;

    case ARequestMethod of
      TRESTRequestMethodType.rtPOST:
        begin
          DoPOST(AResource, ASubResource);
        end;
      TRESTRequestMethodType.rtPUT:
        begin
          DoPUT(AResource, ASubResource);
        end;
      TRESTRequestMethodType.rtGET:
        begin
          DoGET(AResource, ASubResource);
        end;
      TRESTRequestMethodType.rtDELETE:
        begin
          DoDELETE(AResource, ASubResource);
        end;
      TRESTRequestMethodType.rtPATCH: ;
    end;
    // Passao JSON para a VAR que poderá ser manipulada no evento AfterCommand
    FResponseString := Result;
    // DoAfterCommand
    DoAfterCommand;
    // Pega de volta o JSON manipulado ou não no evento AfterCommand
    Result := FResponseString;
  finally
    FResponseString := '';
    FParams.Clear;
    FQueryParams.Clear;
    FBodyParams.Clear;
  end;
end;

function TRESTClientMARS.RemoveContextServerUse(const Value: String): string;
begin
  Result := ReplaceStr(Value, '/ormbr', '');
end;

procedure TRESTClientMARS.SetAuthenticatorTypeValues;
begin
  case FAuthenticator.AuthenticatorType of
    atNoAuth:;
    atBasicAuth:;
    atBearerToken,
    atOAuth1,
    atOAuth2:
      begin
        if Length(FAuthenticator.Token) > 0 then
        begin
//          FRESTClient.Request.HeaderFields.AddPair('Authorization', 'Bearer ' + FAuthenticator.Token);
          Exit;
        end;
      end;
  end;
  FRESTToken.UserName := FAuthenticator.Username;
  FRESTToken.Password := FAuthenticator.Password;
end;

procedure TRESTClientMARS.SetBaseURL;
begin
  inherited;
  FBaseURL := FBaseURL + FRESTContext;
end;

procedure TRESTClientMARS.SetParamValues;
var
  LFor: Integer;
begin
  /// <summary> Params </summary>
  for LFor := 0 to FParams.Count -1 do
    FRESTResource.PathParamsValues.Add(FParams.Items[LFor].AsString);
  /// <summary> Query Params </summary>
  for LFor := 0 to FQueryParams.Count -1 do
    FRESTResource.QueryParams.Add(FQueryParams.Items[LFor].AsString);
end;

procedure TRESTClientMARS.SetParamsBodyValue(var AParams: String);
var
  LFor: Integer;
begin
  if FBodyParams.Count = 0 then
    raise Exception.Create('Não foi passado o parâmetro com os dados do insert!');

  for LFor := 0 to FBodyParams.Count -1 do
    AParams := AParams + FBodyParams.Items[LFor].AsString;
end;

procedure TRESTClientMARS.SetProxyParamsClientValue;
begin
  FRESTClient.HttpClient.ProxyParams.BasicAuthentication := FProxyParams.BasicAuthentication;
  FRESTClient.HttpClient.ProxyParams.ProxyServer := FProxyParams.ProxyServer;
  FRESTClient.HttpClient.ProxyParams.ProxyPort := FProxyParams.ProxyPort;
  FRESTClient.HttpClient.ProxyParams.ProxyUsername := FProxyParams.ProxyUsername;
  FRESTClient.HttpClient.ProxyParams.ProxyPassword := FProxyParams.ProxyPassword;
end;

end.

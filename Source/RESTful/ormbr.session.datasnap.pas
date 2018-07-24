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

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

{$INCLUDE ..\ormbr.inc}

unit ormbr.session.datasnap;

interface

uses
  DB,
  Rtti,
  TypInfo,
  Classes,
  Variants,
  SysUtils,
  Generics.Collections,
  REST.Client,
  /// orm
  ormbr.mapping.explorerstrategy,
  ormbr.dataset.base.adapter,
  ormbr.session.abstract, 
  ormbr.session.baseurl;

type
  /// <summary>
  /// M - Sessão RESTFull
  /// </summary>
  TSessionDataSnap<M: class, constructor> = class(TSessionAbstract<M>)
  private
    FRESTResponse: TRESTResponse;
    FRESTRequest: TRESTRequest;
    FRESTClient: TRESTClient;
    FResource: String;
  public
    constructor Create(const APageSize: Integer = -1); override;
    destructor Destroy; override;
    procedure Insert(const AObject: M); overload; override;
    procedure Update(const AObjectList: TObjectList<M>); overload; override;
    procedure Delete(const AID: Integer); overload; override;
    procedure Delete(const AObject: M); overload; override;
    function ExistSequence: Boolean; override;
    function Find: TObjectList<M>; overload; override;
    function Find(const AID: Integer): M; overload; override;
    function Find(const AID: String): M; overload; override;
    function FindWhere(const AWhere: string; const AOrderBy: string = ''): TObjectList<M>; override;
  end;

implementation

uses
  REST.Types,
  IPPeerClient,
  DBXJSONReflect,
  JSON,
  ormbr.rest.json,
  ormbr.objects.helper,
  ormbr.mapping.classes,
  ormbr.mapping.attributes,
  ormbr.restdataset.adapter,
  ormbr.json.utils;

{ TSessionDataSnap<M> }

constructor TSessionDataSnap<M>.Create(const APageSize: Integer = -1);
var
  LObject: TObject;
  ABaseURL: String;
  LTable: TCustomAttribute;
  LResource: TCustomAttribute;
begin
  inherited Create(APageSize);
  /// <summary>
  ///  Verifica se foi informado a URL no Singleton
  /// </summary>
  ABaseURL := TSessionRESTBaseURL.GetInstance.BaseURL;
  if Length(ABaseURL) = 0 then
    raise Exception.Create('Defina a URL base na classe sington [TSessionRESTBaseURL.GetInstance.BaseURL := "http://127.0.0.1:211/datasnap/rest/tormbr"]');

  FRESTRequest := TRESTRequest.Create(nil);
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTClient := TRESTClient.Create(ABaseURL);
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
  FRESTResponse.RootElement := 'result';
  /// <summary>
  /// Pega o nome do recurso, caso não encontre o atributo Resource(),
  /// internamente busca pelo atributo Table()
  /// </summary>
  LObject := TObject(M.Create);
  try
    LResource := LObject.GetResource;
    if LResource <> nil then
      FResource := Resource(LResource).Name;

    if FResource = '' then
    begin
      LTable := LObject.GetTable;
      if LTable <> nil then
        FResource := Table(LTable).Name;
    end;
  finally
    LObject.Free;
  end;
end;

procedure TSessionDataSnap<M>.Delete(const AObject: M);
var
  LColumn: TColumnMapping;
begin
  for LColumn in AObject.GetPrimaryKey do
    Delete(LColumn.PropertyRtti.GetValue(TObject(AObject)).AsInteger);
end;

destructor TSessionDataSnap<M>.Destroy;
begin
  FRESTClient.Free;
  FRESTResponse.Free;
  FRESTRequest.Free;
  inherited;
end;

function TSessionDataSnap<M>.ExistSequence: Boolean;
begin
  Result := False;
end;

procedure TSessionDataSnap<M>.Delete(const AID: Integer);
begin
  FRESTRequest.ResetToDefaults;
  FRESTRequest.Resource := '/' + FResource + '/{ID}';
  FRESTRequest.Method := TRESTRequestMethod.rmDELETE;
  FRESTRequest.Params.AddUrlSegment('ID', IntToStr(AID));
  FRESTRequest.Execute;
end;

function TSessionDataSnap<M>.FindWhere(const AWhere, AOrderBy: string): TObjectList<M>;
var
  LJSON: string;
begin
  FRESTRequest.ResetToDefaults;
  FRESTRequest.Resource := '/' + FResource + 'Where' + '/{WHERE}/{ORDERBY}';
  FRESTRequest.Method := TRESTRequestMethod.rmGET;
  FRESTRequest.Params.AddUrlSegment('WHERE', AWhere);
  FRESTRequest.Params.AddUrlSegment('ORDERBY', AOrderBy);
  FRESTRequest.Execute;

  LJSON := TJSONArray(FRESTRequest.Response.JSONValue).Items[0].ToJSON;
  /// <summary>
  /// Transforma o JSON recebido populando o objeto
  /// </summary>
  Result := TORMBrJson.JsonToObjectList<M>(LJSON);
end;

function TSessionDataSnap<M>.Find(const AID: Integer): M;
begin
  /// <summary>
  /// Transforma o JSON recebido populando o objeto
  /// </summary>
  Result := Find(IntToStr(AID));
end;

function TSessionDataSnap<M>.Find(const AID: string): M;
var
  LJSON: string;
begin
  FRESTRequest.ResetToDefaults;
  FRESTRequest.Resource := '/' + FResource + '/{ID}';
  FRESTRequest.Method := TRESTRequestMethod.rmGET;
  FRESTRequest.Params.AddUrlSegment('ID', AID);
  FRESTRequest.Execute;

  LJSON := TJSONArray(TJSONArray(FRESTRequest.Response.JSONValue).Items[0]).Items[0].ToJSON;
  /// <summary>
  /// Transforma o JSON recebido populando o objeto
  /// </summary>
  Result := TORMBrJson.JsonToObject<M>(LJSON);
end;

function TSessionDataSnap<M>.Find: TObjectList<M>;
var
  LJSON: string;
begin
  FRESTRequest.ResetToDefaults;
  FRESTRequest.Resource := '/' + FResource + '/{ID}';
  FRESTRequest.Method := TRESTRequestMethod.rmGET;
  FRESTRequest.Params.AddUrlSegment('ID', '0');
  FRESTRequest.Execute;

  LJSON := TJSONArray(FRESTRequest.Response.JSONValue).Items[0].ToJSON;
  /// <summary>
  /// Transforma o JSON recebido populando o objeto
  /// </summary>
  Result := TORMBrJson.JsonToObjectList<M>(LJSON);
end;

procedure TSessionDataSnap<M>.Insert(const AObject: M);
var
  FJSON: String;
begin
  FJSON := TORMBrJson.ObjectToJsonString(AObject);

  FRESTRequest.ResetToDefaults;
  FRESTRequest.Resource := '/' + FResource;
  FRESTRequest.Method := TRESTRequestMethod.rmPUT;
  {$IFDEF DELPHI22_UP}
  FRESTRequest.AddBody(FJSON, ContentTypeFromString('application/json'));
  {$ELSE}
  FRESTRequest.Body.Add(FJSON, ContentTypeFromString('application/json'));
  {$ENDIF}
  FRESTRequest.Execute;
end;

procedure TSessionDataSnap<M>.Update(const AObjectList: TObjectList<M>);
var
  FJSON: TJSONArray;
begin
  FJSON := TORMBrJSONUtil.JSONObjectListToJSONArray<M>(AObjectList);
  try
    FRESTRequest.ResetToDefaults;
    FRESTRequest.Resource := '/' + FResource;
    FRESTRequest.Method := TRESTRequestMethod.rmPOST;
    {$IFDEF DELPHI22_UP}
    FRESTRequest.AddBody(FJSON.ToJSON, ContentTypeFromString('application/json'));
    {$ELSE}
    FRESTRequest.Body.Add(FJSON.ToJSON, ContentTypeFromString('application/json'));
    {$ENDIF}
    FRESTRequest.Execute;
  finally
    FJSON.Free;
  end;
end;

end.

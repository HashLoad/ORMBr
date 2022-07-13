{
      ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
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

{$INCLUDE ..\..\ormbr.inc}

unit ormbr.session.rest;

interface

uses
  DB,
  Rtti,
  TypInfo,
  Classes,
  Variants,
  SysUtils,
  StrUtils,
  Generics.Collections,
  {$IFDEF DELPHI15_UP}
  JSON,
  {$ELSE}
  DBXJSON,
  {$ENDIF}
  // ORMBr
  ormbr.dataset.base.adapter,
  ormbr.session.abstract,
  dbebr.factory.interfaces,
  ormbr.client.methods,
  ormbr.client.interfaces,
  ormbr.restdataset.adapter;

type
  // M - Sessão RESTFull
  TSessionRest<M: class, constructor> = class(TSessionAbstract<M>)
  private
    FOwner: TRESTDataSetAdapter<M>;
    FConnection: IRESTConnection;
    FResource: String;
    FSubResource: String;
    FServerUse: Boolean;
    function NextPacketMethod: TObjectList<M>; overload;
    function NextPacketMethod(AWhere, AOrderBy: String): TObjectList<M>; overload;
    function ParseOperator(AParams: String): string;
  public
    constructor Create(const AConnection: IRESTConnection;
      const AOwner: TRESTDataSetAdapter<M>; const APageSize: Integer = -1); overload;
    destructor Destroy; override;
    procedure Insert(const AObject: M); overload; override;
    procedure Update(const AObjectList: TObjectList<M>); overload; override;
    procedure Delete(const AID: Integer); overload; override;
    procedure Delete(const AObject: M); overload; override;
    procedure RefreshRecord(const AColumns: TParams); override;
    procedure NextPacketList(const AObjectList: TObjectList<M>); overload; override;
    function NextPacketList: TObjectList<M>; overload; override;
    function Find: TObjectList<M>; overload; override;
    function Find(const AID: Int64): M; overload; override;
    function Find(const AID: String): M; overload; override;
    function FindWhere(const AWhere: string; const AOrderBy: string = ''): TObjectList<M>; override;
    function ExistSequence: Boolean; override;
    function Find(const AMethodName: String;
      const AParams: array of string): TObjectList<M>; overload; //override;
  end;

implementation

uses
  ormbr.json,
  ormbr.core.consts,
  ormbr.objects.utils,
  ormbr.objects.helper,
  dbcbr.mapping.classes,
  dbcbr.mapping.explorer,
  dbcbr.mapping.attributes;

{ TSessionRest<M> }

constructor TSessionRest<M>.Create(const AConnection: IRESTConnection;
  const AOwner: TRESTDataSetAdapter<M>; const APageSize: Integer = -1);
var
  LObject: TObject;
  LTable: TCustomAttribute;
  LResource: TCustomAttribute;
  LSubResource: TCustomAttribute;
  LNotServerUse: TCustomAttribute;
begin
  inherited Create(APageSize);
  FOwner := AOwner;
  FConnection := AConnection;
  FPageSize := APageSize;
  FPageNext := 0;
  FFindWhereUsed := False;
  FFindWhereRefreshUsed := False;
  FResource := '';
  FSubResource := '';
  FServerUse := False;
  // Pega o nome do recurso e subresource definidos na classe
  LObject := TObject(M.Create);
  try
    if FConnection.ServerUse then
    begin
      // Valida se tem o atributo NotServerUse para não usar o server
      LNotServerUse := LObject.GetNotServerUse;
      if LNotServerUse <> nil then
      begin
        FServerUse := False;
        FConnection.SetClassNotServerUse(True);
      end
      else
      begin
        FServerUse := True;
        FConnection.SetClassNotServerUse(False);
      end;
      LTable := LObject.GetTable;
      if LTable <> nil then
        FResource := Table(LTable).Name;
    end
    else
    begin
      // Nome do Recurso
      LResource := LObject.GetResource;
      if LResource <> nil then
        FResource := Resource(LResource).Name;

      if LResource = nil then
      begin
        LTable := LObject.GetTable;
        if LTable <> nil then
          FResource := Table(LTable).Name;
      end;
      // Nome do SubRecurso
      LSubResource := LObject.GetSubResource;
      if LSubResource <> nil then
        FSubResource := Resource(LSubResource).Name;
    end;
  finally
    LObject.Free;
  end;
end;

destructor TSessionRest<M>.Destroy;
begin
  inherited;
end;

function TSessionRest<M>.ExistSequence: Boolean;
var
  LSequence: TSequenceMapping;
begin
  Result := False;
  LSequence := TMappingExplorer.GetMappingSequence(TClass(M));
  if LSequence <> nil then
    Result := True;
end;

procedure TSessionRest<M>.Delete(const AObject: M);
var
  LColumn: TColumnMapping;
  LPrimaryKey: TPrimaryKeyColumnsMapping;
begin
  LPrimaryKey := TMappingExplorer.GetMappingPrimaryKeyColumns(AObject.ClassType);
  if LPrimaryKey = nil then
    raise Exception.Create(cMESSAGEPKNOTFOUND);

  LColumn := LPrimaryKey.Columns.Items[0];
  Delete(LColumn.ColumnProperty.GetValue(TObject(AObject)).AsInteger);
end;

procedure TSessionRest<M>.Delete(const AID: Integer);
var
  LSubResource: String;
  LURI: String;
  LResult: String;
  LResource: String;
begin
  LResource := FResource;
  // Só concatena o ID na URI se a propriedade ServerUse for igual a TRUE,
  // caso contrário será passado como parâmetro
  if FServerUse then
    LResource := LResource + '(' + IntToStr(AID) + ')';
  LSubResource := ifThen(Length(FConnection.MethodDELETE) > 0, FConnection.MethodDELETE, FSubResource);
  try
    LResult := FConnection.Execute(LResource,
                                   LSubResource,
                                   rtDELETE,
                                   procedure
                                   begin
                                     if not FServerUse then
                                       FConnection.AddQueryParam('$value=' + IntToStr(AID));
                                   end);
  finally
    // Mostra no monitor a URI completa
    if FConnection.CommandMonitor <> nil then
    begin
      LURI := FConnection.FullURL;
      FConnection.CommandMonitor.Command('URI    : ' + LURI + sLineBreak +
                                         'ID     : ' + IntToStr(AID) + sLineBreak +
                                         'Método : DELETE' + sLineBreak +
                                         'Result : ' + LResult, nil);
    end;
  end;
end;

function TSessionRest<M>.FindWhere(const AWhere, AOrderBy: string): TObjectList<M>;
var
  LSubResource: String;
  LJSON: string;
  LURI: String;
begin
  FFindWhereUsed := True;
  FFetchingRecords := False;
  FWhere := AWhere;
  FOrderBy := AOrderBy;
  // Só busca por paginação se não for um RefreshRecord
  if not FFindWhereRefreshUsed then
  begin
    if FPageSize > -1 then
    begin
      FPageNext := 0 - FPageSize;
      Result := NextPacketMethod(FWhere, FOrderBy);
      Exit;
    end;
  end;
  LSubResource := '';
  if not FServerUse then
    LSubResource := ifThen(Length(FConnection.MethodGETWhere) > 0, FConnection.MethodGETWhere, FSubResource);

  try
    LJSON := FConnection.Execute(FResource,
                                 LSubResource,
                                 rtGET,
                                 procedure
                                 begin
                                   FConnection.AddQueryParam('$filter=' + ParseOperator(FWhere));
                                   if Length(FOrderBy) > 0 then
                                     FConnection.AddQueryParam('$orderby=' + FOrderBy);
                                 end);
    // Caso o JSON retornado não seja um array, é tranformado em um.
    if {$IFDEF NEXTGEN}LJSON[0]{$ELSE}LJSON[1]{$ENDIF} = '{' then
      LJSON := '[' + LJSON + ']';

    // Transforma o JSON recebido populando em uma lista de objetos
    Result := TORMBrJson.JsonToObjectList<M>(LJSON);
  finally
    // Mostra no monitor a URI completa
    if FConnection.CommandMonitor <> nil then
    begin
      LURI := FConnection.FullURL;
      FConnection.CommandMonitor.Command('URI    : ' + LURI + sLineBreak +
                                         'Where  : ' + AWhere + sLineBreak +
                                         'OrderBy: ' + AOrderBy + sLineBreak +
                                         'Método : GET' + sLineBreak +
                                         'Json   : ' + LJSON, nil);
    end;
  end;
end;

function TSessionRest<M>.Find(const AID: Int64): M;
begin
  // Transforma o JSON recebido populando o objeto
  FFindWhereUsed := False;
  FFetchingRecords := False;
  Result := Find(IntToStr(AID));
end;

function TSessionRest<M>.Find(const AID: string): M;
var
  LResource: String;
  LSubResource: String;
  LJSON: String;
  LURI: String;
begin
  FFindWhereUsed := False;
  FFetchingRecords := False;
  LResource := FResource;
  if not FServerUse then
    LSubResource := FConnection.MethodGETId
  else
  begin
    LResource := LResource + '(' + AID + ')';
    LSubResource := '';
  end;
  try
    LJSON := FConnection.Execute(LResource,
                                 LSubResource,
                                 rtGET,
                                 procedure
                                 begin
                                   if not FServerUse then
                                     FConnection.AddQueryParam('$value=' + AID)
                                 end);
    // Transforma o JSON recebido populando o objeto
    Result := TORMBrJson.JsonToObject<M>(LJSON);
  finally
    // ostra no monitor a URI completa
    if FConnection.CommandMonitor <> nil then
    begin
      LURI := FConnection.FullURL;
      FConnection.CommandMonitor.Command('URI    : ' + LURI + sLineBreak +
                                         'ID     : ' + AID  + sLineBreak +
                                         'Método : GET' + sLineBreak +
                                         'Json   : ' + LJSON, nil);
    end;
  end;
end;

function TSessionRest<M>.Find: TObjectList<M>;
var
  LJSON: string;
  LSubResource: String;
  LURI: String;
begin
  FFetchingRecords := False;
  FFindWhereUsed := False;
  if FPageSize > -1 then
  begin
    FPageNext := 0 - FPageSize;
    Result := NextPacketMethod;
    Exit;
  end;
  LSubResource := '';
  if not FServerUse then
    LSubResource := ifThen(Length(FConnection.MethodGET) > 0, FConnection.MethodGET, FSubResource);
  try
    LJSON := FConnection.Execute(FResource, LSubResource, rtGET);
    // Caso o JSON retornado não seja um array, é tranformado em um.
    if {$IFDEF NEXTGEN}LJSON[0]{$ELSE}LJSON[1]{$ENDIF} = '{' then
      LJSON := '[' + LJSON + ']';

    // Transforma o JSON recebido populando uma lista de objetos
    Result := TORMBrJson.JsonToObjectList<M>(LJSON);
  finally
    // Mostra no monitor a URI completa
    if FConnection.CommandMonitor <> nil then
    begin
      LURI := FConnection.FullURL;
      FConnection.CommandMonitor.Command('URI    : ' + LURI + sLineBreak +
                                         'Método : GET' + sLineBreak +
                                         'Json   : ' + LJSON, nil);
    end;
  end;
end;

procedure TSessionRest<M>.Insert(const AObject: M);
var
  LJSON: String;
  LSubResource: String;
  LURI: String;
  LResult: String;
  LParamsObject: TJSONObject;
  LParamsArray: TJSONArray;
  LValuesObject: TJSONObject;
  LFor: Integer;
  LPar: Integer;
begin
  LSubResource := ifThen(Length(FConnection.MethodPOST) > 0, FConnection.MethodPOST, FSubResource);
  LJSON := TORMBrJson.ObjectToJsonString(AObject);
  try
    LResult := FConnection.Execute(FResource,
                                   LSubResource,
                                   rtPOST,
                                   procedure
                                   begin
                                     FConnection.AddBodyParam(LJSON);
                                   end);
    FResultParams.Clear;
    // Gera lista de params com o retorno, se existir o elemento "params" no JSON.
    LParamsObject := TORMBrJson.JSONStringToJSONObject(LResult);
    if LParamsObject = nil then
      Exit;

    LParamsArray := LParamsObject.Values['params'] as TJSONArray;
    if LParamsArray = nil then
      Exit;

    for LFor := 0 to LParamsArray.Count -1 do
    begin
      LValuesObject := LParamsArray.Items[LFor] as TJSONObject;
      with FResultParams.Add as TParam do
      begin
        for LPar := 0 to LValuesObject.Count -1 do
        begin
          Name := LValuesObject.Pairs[LPar].JsonString.Value;
          DataType := ftString;
          Value := LValuesObject.Pairs[LPar].JsonValue.Value
        end;
      end;
    end;
  finally
    if LParamsObject <> nil then
      LParamsObject.Free;
    // Mostra no monitor a URI completa
    if FConnection.CommandMonitor <> nil then
    begin
      LURI := FConnection.FullURL;
      FConnection.CommandMonitor.Command('URI    : ' + LURI + sLineBreak +
                                         'Método : POST' + sLineBreak +
                                         'Result : ' + LResult + sLineBreak +
                                         'Json   : ' + LJSON, nil);
    end;
  end;
end;

function TSessionRest<M>.NextPacketList: TObjectList<M>;
begin
  inherited;
  if FFindWhereUsed then
    Result := NextPacketMethod(FWhere, FOrderBy)
  else
    Result := NextPacketMethod;
  if Result = nil then
    Exit;
  if Result.Count > 0 then
    Exit;
  FFetchingRecords := True;
end;

procedure TSessionRest<M>.NextPacketList(const AObjectList: TObjectList<M>);
var
  LObjectList: TObjectList<M>;
  LFor: Integer;
  LObject: TObject;
begin
  if FFindWhereUsed then
    LObjectList := NextPacketMethod(FWhere, FOrderBy)
  else
    LObjectList := NextPacketMethod;
  if LObjectList = nil then
    Exit;
  if LObjectList.Count = 0 then
    FFetchingRecords := True;
  try
    for LFor := 0 to LObjectList.Count -1 do
    begin
      LObject := TRttiSingleton.GetInstance.Clone(LObjectList.Items[LFor]);
      AObjectList.Add(LObject);
    end;
  finally
    LObjectList.Clear;
    LObjectList.Free;
  end;
end;

function TSessionRest<M>.NextPacketMethod(AWhere, AOrderBy: String): TObjectList<M>;
var
  LJSON: string;
  LSubResource: String;
  LURI: String;
begin
  if not FFindWhereRefreshUsed then
    FPageNext := FPageNext + FPageSize;

  LSubResource := '';
  if not FServerUse then
    LSubResource := ifThen(Length(FConnection.MethodGETNextPacketWhere) > 0, FConnection.MethodGETNextPacketWhere, FSubResource);
  try
    LJSON := FConnection.Execute(FResource,
                                 LSubResource,
                                 rtGET,
                                 procedure
                                 begin
                                   FConnection.AddQueryParam('$filter='  + ParseOperator(AWhere));
                                   FConnection.AddQueryParam('$orderby=' + AOrderBy);
                                   FConnection.AddQueryParam('$top='     + IntToStr(FPageSize));
                                   FConnection.AddQueryParam('$skip='    + IntToStr(FPageNext));
                                 end);
    // Transforma o JSON recebido populando o objeto
    Result := TORMBrJson.JsonToObjectList<M>(LJSON);
  finally
    // Mostra no monitor a URI completa
    if FConnection.CommandMonitor <> nil then
    begin
      LURI := FConnection.FullURL;
      if Length(LSubResource) > 0 then
        LURI := LURI + '/' + LSubResource;

      FConnection.CommandMonitor.Command('URI    : ' + LURI + sLineBreak +
                                         'Método : GET' + sLineBreak +
                                         'Json   : ' + LJSON, nil);
    end;
  end;
end;

function TSessionRest<M>.NextPacketMethod: TObjectList<M>;
var
  LJSON: string;
  LSubResource: String;
  LURI: String;
begin
  FPageNext := FPageNext + FPageSize;
  LSubResource := '';
  if not FServerUse then
    LSubResource := ifThen(Length(FConnection.MethodGETNextPacket) > 0, FConnection.MethodGETNextPacket, FSubResource);
  try
    LJSON := FConnection.Execute(FResource,
                                 LSubResource,
                                 rtGET,
                                 procedure
                                 begin
                                   FConnection.AddQueryParam('$top='  + IntToStr(FPageSize));
                                   FConnection.AddQueryParam('$skip=' + IntToStr(FPageNext));
                                 end);
    // Transforma o JSON recebido populando o objeto
    Result := TORMBrJson.JsonToObjectList<M>(LJSON);
  finally
    // Mostra no monitor a URI completa
    if FConnection.CommandMonitor <> nil then
    begin
      LURI := FConnection.FullURL;
      if Length(LSubResource) > 0 then
        LURI := LURI + '/' + LSubResource;

      // Gera Lentidão se tiver campo TBlob no JSON
      FConnection.CommandMonitor.Command('URI    : ' + LURI + sLineBreak +
                                         'Método : GET' + sLineBreak +
                                         'Json   : ' + LJSON, nil);
    end;
  end;
end;

procedure TSessionRest<M>.Update(const AObjectList: TObjectList<M>);
var
  LJSON: String;
  LSubResource: String;
  LURI: String;
  LResult: String;
  LFor: Integer;
  LResource: String;
begin
  LJSON := '';
  LSubResource := ifThen(Length(FConnection.MethodPUT) > 0, FConnection.MethodPUT, FSubResource);
  LResource := FResource;
  try
    for LFor := 0 to AObjectList.Count -1 do
    begin
      LJSON := TORMBrJson.ObjectToJsonString(AObjectList.Items[LFor]);
      LResult := FConnection.Execute(LResource,
                                     LSubResource,
                                     rtPUT,
                                     procedure
                                     begin
                                       FConnection.AddBodyParam(LJSON);
                                     end);
    end;
  finally
    // Mostra no monitor a URI completa
    if FConnection.CommandMonitor <> nil then
    begin
      LURI := FConnection.FullURL;
      FConnection.CommandMonitor.Command('URI    : ' + LURI + sLineBreak +
                                         'Método : PUT' + sLineBreak +
                                         'Result : ' + LResult + sLineBreak +
                                         'Json   : ' + LJSON, nil);
    end;
  end;
end;

procedure TSessionRest<M>.RefreshRecord(const AColumns: TParams);
var
  LObjectList: TObjectList<M>;
  LFindWhere: String;
  LWhereOld: String;
  LOrderByOld: String;
  LFor: Integer;
begin
  inherited;
  FFindWhereRefreshUsed := True;
  LWhereOld := FWhere;
  LOrderByOld := FOrderBy;
  try
    LFindWhere := '';
    for LFor := 0 to AColumns.Count -1 do
    begin
      LFindWhere := LFindWhere + AColumns[LFor].Name + '=' + AColumns[LFor].AsString;
      if LFor < AColumns.Count -1 then
        LFindWhere := LFindWhere + ' AND ';
    end;
    LObjectList := FindWhere(LFindWhere, '');
    if LObjectList = nil then
      Exit;
    try
      FOwner.RefreshRecordInternal(LObjectList.First);
    finally
      LObjectList.Clear;
      LObjectList.Free;
    end;
  finally
    FWhere := LWhereOld;
    FOrderBy := LOrderByOld;
    FFindWhereRefreshUsed := False;
  end;
end;

function TSessionRest<M>.Find(const AMethodName: String;
  const AParams: array of string): TObjectList<M>;
var
  LJSONArray: TJSONArray;
  LFor: Integer;
  LJSON: string;
  LURI: String;
begin
  FFindWhereUsed := False;
  FFetchingRecords := False;
  LJSONArray := TJSONArray.Create;
  try
    try
      for LFor := Low(AParams) to High(AParams) do
        LJSONArray.Add(AParams[LFor]);

      LJSON := FConnection.Execute(FResource,
                                   AMethodName,
                                   rtGET,
                                   procedure
                                   begin
                                     FConnection.AddBodyParam(LJSONArray.ToJSON);
                                   end);
    except
      on E: Exception do
      begin
        raise Exception.Create(E.Message);
      end;
    end;
    // Transforma o JSON recebido populando o objeto
    Result := TORMBrJson.JsonToObjectList<M>(LJSON);
  finally
    LJSONArray.Free;
    // Mostra no monitor a URI completa
    if FConnection.CommandMonitor <> nil then
    begin
      LURI := FConnection.FullURL;
      FConnection.CommandMonitor.Command('URI    : ' + LURI + sLineBreak +
                                         'Método : GET' + sLineBreak +
                                         'Json   : ' + LJSON, nil);
    end;
  end;
end;

function TSessionRest<M>.ParseOperator(AParams: String): string;
begin
  Result := AParams;
  Result := StringReplace(Result, ' = ' , ' eq ' , [rfReplaceAll]);
  Result := StringReplace(Result, ' <> ', ' ne ' , [rfReplaceAll]);
  Result := StringReplace(Result, ' > ' , ' gt ' , [rfReplaceAll]);
  Result := StringReplace(Result, ' >= ', ' ge ' , [rfReplaceAll]);
  Result := StringReplace(Result, ' < ' , ' lt ' , [rfReplaceAll]);
  Result := StringReplace(Result, ' <= ', ' le ' , [rfReplaceAll]);
  Result := StringReplace(Result, ' + ' , ' add ', [rfReplaceAll]);
  Result := StringReplace(Result, ' - ' , ' sub ', [rfReplaceAll]);
  Result := StringReplace(Result, ' * ' , ' mul ', [rfReplaceAll]);
  Result := StringReplace(Result, ' / ' , ' div ', [rfReplaceAll]);
end;

end.

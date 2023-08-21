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

{$INCLUDE ..\..\ormbr.inc}

unit ormbr.session.restful;

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
  ormbr.session.abstract,
  dbebr.factory.interfaces,
  ormbr.client.methods,
  ormbr.restfactory.interfaces,
  ormbr.restdataset.adapter;

type
  TSessionRestFul<M: class, constructor> = class(TSessionAbstract<M>)
  private
    FOwner: TRESTDataSetAdapter<M>;
    FConnection: IRESTConnection;
    FResource: String;
    FSubResource: String;
    FServerUse: Boolean;
    function _NextPacketMethod: TObjectList<M>; overload;
    function _NextPacketMethod(AWhere, AOrderBy: String): TObjectList<M>; overload;
    function _ParseOperator(AParams: String): string;
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
    {$IFDEF DRIVERRESTFUL}
    function Find(const AMethodName: String;
      const AParams: array of string): TObjectList<M>; overload; override;
    {$ENDIF}
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

constructor TSessionRestFul<M>.Create(const AConnection: IRESTConnection;
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

destructor TSessionRestFul<M>.Destroy;
begin

  inherited;
end;

function TSessionRestFul<M>.ExistSequence: Boolean;
var
  LSequence: TSequenceMapping;
begin
  Result := False;
  LSequence := TMappingExplorer.GetMappingSequence(TClass(M));
  if LSequence <> nil then
    Result := True;
end;

procedure TSessionRestFul<M>.Delete(const AObject: M);
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

procedure TSessionRestFul<M>.Delete(const AID: Integer);
var
  LSubResource: String;
  LURL: String;
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
                                   TRESTRequestMethodType.rtDELETE,
                                   procedure
                                   begin
                                     if not FServerUse then
                                       FConnection.AddQueryParam('$value=' + IntToStr(AID));
                                   end);
  finally
    // Mostra no monitor a URI completa
    if FConnection.CommandMonitor <> nil then
    begin
      LURL := FConnection.FullURL;
      FConnection.CommandMonitor.Command('URI    : ' + LURL + sLineBreak +
                                         'ID     : ' + IntToStr(AID) + sLineBreak +
                                         'Método : DELETE' + sLineBreak +
                                         'Result : ' + LResult, nil);
    end;
  end;
end;

function TSessionRestFul<M>.FindWhere(const AWhere, AOrderBy: string): TObjectList<M>;
var
  LSubResource: String;
  LJSON: string;
  LURL: String;
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
      Result := _NextPacketMethod(FWhere, FOrderBy);
      Exit;
    end;
  end;
  LSubResource := '';
  if not FServerUse then
    LSubResource := ifThen(Length(FConnection.MethodGETWhere) > 0, FConnection.MethodGETWhere, FSubResource);

  try
    LJSON := FConnection.Execute(FResource,
                                 LSubResource,
                                 TRESTRequestMethodType.rtGET,
                                 procedure
                                 begin
                                   FConnection.AddQueryParam('$filter=' + _ParseOperator(FWhere));
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
      LURL := FConnection.FullURL;
      FConnection.CommandMonitor.Command('URI    : ' + LURL + sLineBreak +
                                         'Where  : ' + AWhere + sLineBreak +
                                         'OrderBy: ' + AOrderBy + sLineBreak +
                                         'Método : GET' + sLineBreak +
                                         'Json   : ' + LJSON, nil);
    end;
  end;
end;

function TSessionRestFul<M>.Find(const AID: Int64): M;
begin
  // Transforma o JSON recebido populando o objeto
  FFindWhereUsed := False;
  FFetchingRecords := False;
  Result := Find(IntToStr(AID));
end;

function TSessionRestFul<M>.Find(const AID: string): M;
var
  LResource: String;
  LSubResource: String;
  LJSON: String;
  LURL: String;
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
                                 TRESTRequestMethodType.rtGET,
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
      LURL := FConnection.FullURL;
      FConnection.CommandMonitor.Command('URI    : ' + LURL + sLineBreak +
                                         'ID     : ' + AID  + sLineBreak +
                                         'Método : GET' + sLineBreak +
                                         'Json   : ' + LJSON, nil);
    end;
  end;
end;

function TSessionRestFul<M>.Find: TObjectList<M>;
var
  LJSON: string;
  LSubResource: String;
  LURL: String;
begin
  FFetchingRecords := False;
  FFindWhereUsed := False;
  if FPageSize > -1 then
  begin
    FPageNext := 0 - FPageSize;
    Result := _NextPacketMethod;
    Exit;
  end;
  LSubResource := '';
  if not FServerUse then
    LSubResource := ifThen(Length(FConnection.MethodGET) > 0, FConnection.MethodGET, FSubResource);
  try
    LJSON := FConnection.Execute(FResource, LSubResource, TRESTRequestMethodType.rtGET);
    // Caso o JSON retornado não seja um array, é tranformado em um.
    if {$IFDEF NEXTGEN}LJSON[0]{$ELSE}LJSON[1]{$ENDIF} = '{' then
      LJSON := '[' + LJSON + ']';

    // Transforma o JSON recebido populando uma lista de objetos
    Result := TORMBrJson.JsonToObjectList<M>(LJSON);
  finally
    // Mostra no monitor a URI completa
    if FConnection.CommandMonitor <> nil then
    begin
      LURL := FConnection.FullURL;
      FConnection.CommandMonitor.Command('URI    : ' + LURL + sLineBreak +
                                         'Método : GET' + sLineBreak +
                                         'Json   : ' + LJSON, nil);
    end;
  end;
end;

procedure TSessionRestFul<M>.Insert(const AObject: M);
var
  LJSON: String;
  LSubResource: String;
  LURL: String;
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
                                   TRESTRequestMethodType.rtPOST,
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
      LURL := FConnection.FullURL;
      FConnection.CommandMonitor.Command('URI    : ' + LURL + sLineBreak +
                                         'Método : POST' + sLineBreak +
                                         'Result : ' + LResult + sLineBreak +
                                         'Json   : ' + LJSON, nil);
    end;
  end;
end;

function TSessionRestFul<M>.NextPacketList: TObjectList<M>;
begin
  if FFindWhereUsed then
    Result := _NextPacketMethod(FWhere, FOrderBy)
  else
    Result := _NextPacketMethod;
  if Result = nil then
    Exit;
  if Result.Count > 0 then
    Exit;
  FFetchingRecords := True;
end;

procedure TSessionRestFul<M>.NextPacketList(const AObjectList: TObjectList<M>);
var
  LObjectList: TObjectList<M>;
  LFor: Integer;
  LObject: TObject;
begin
  if FFindWhereUsed then
    LObjectList := _NextPacketMethod(FWhere, FOrderBy)
  else
    LObjectList := _NextPacketMethod;
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

function TSessionRestFul<M>._NextPacketMethod(AWhere, AOrderBy: String): TObjectList<M>;
var
  LJSON: string;
  LSubResource: String;
  LURL: String;
begin
  if not FFindWhereRefreshUsed then
    FPageNext := FPageNext + FPageSize;

  LSubResource := '';
  if not FServerUse then
    LSubResource := ifThen(Length(FConnection.MethodGETNextPacketWhere) > 0, FConnection.MethodGETNextPacketWhere, FSubResource);
  try
    LJSON := FConnection.Execute(FResource,
                                 LSubResource,
                                 TRESTRequestMethodType.rtGET,
                                 procedure
                                 begin
                                   FConnection.AddQueryParam('$filter='  + _ParseOperator(AWhere));
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
      LURL := FConnection.FullURL;
      if Length(LSubResource) > 0 then
        LURL := LURL + '/' + LSubResource;

      FConnection.CommandMonitor.Command('URI    : ' + LURL + sLineBreak +
                                         'Método : GET' + sLineBreak +
                                         'Json   : ' + LJSON, nil);
    end;
  end;
end;

function TSessionRestFul<M>._NextPacketMethod: TObjectList<M>;
var
  LJSON: string;
  LSubResource: String;
  LURL: String;
begin
  FPageNext := FPageNext + FPageSize;
  LSubResource := '';
  if not FServerUse then
    LSubResource := ifThen(Length(FConnection.MethodGETNextPacket) > 0, FConnection.MethodGETNextPacket, FSubResource);
  try
    LJSON := FConnection.Execute(FResource,
                                 LSubResource,
                                 TRESTRequestMethodType.rtGET,
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
      LURL := FConnection.FullURL;
      if Length(LSubResource) > 0 then
        LURL := LURL + '/' + LSubResource;

      // Gera Lentidão se tiver campo TBlob no JSON
      FConnection.CommandMonitor.Command('URI    : ' + LURL + sLineBreak +
                                         'Método : GET' + sLineBreak +
                                         'Json   : ' + LJSON, nil);
    end;
  end;
end;

procedure TSessionRestFul<M>.Update(const AObjectList: TObjectList<M>);
var
  LJSON: String;
  LSubResource: String;
  LURL: String;
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
                                     TRESTRequestMethodType.rtPUT,
                                     procedure
                                     begin
                                       FConnection.AddBodyParam(LJSON);
                                     end);
    end;
  finally
    // Mostra no monitor a URI completa
    if FConnection.CommandMonitor <> nil then
    begin
      LURL := FConnection.FullURL;
      FConnection.CommandMonitor.Command('URI    : ' + LURL + sLineBreak +
                                         'Método : PUT' + sLineBreak +
                                         'Result : ' + LResult + sLineBreak +
                                         'Json   : ' + LJSON, nil);
    end;
  end;
end;

procedure TSessionRestFul<M>.RefreshRecord(const AColumns: TParams);
var
  LObjectList: TObjectList<M>;
  LFindWhere: String;
  LWhereOld: String;
  LOrderByOld: String;
  LFor: Integer;
begin
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

{$IFDEF DRIVERRESTFUL}
function TSessionRestFul<M>.Find(const AMethodName: String;
  const AParams: array of string): TObjectList<M>;
var
  LJSONArray: TJSONArray;
  LFor: Integer;
  LJSON: string;
  LURL: String;
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
                                   TRESTRequestMethodType.rtGET,
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
      LURL := FConnection.FullURL;
      FConnection.CommandMonitor.Command('URI    : ' + LURL + sLineBreak +
                                         'Método : GET' + sLineBreak +
                                         'Json   : ' + LJSON, nil);
    end;
  end;
end;
{$ENDIF}

function TSessionRestFul<M>._ParseOperator(AParams: String): string;
const
  OPERATORMAP: array[0..9, 0..1] of string = ( (' = ', ' eq '),
                                               (' <> ', ' ne '),
                                               (' > ', ' gt '),
                                               (' >= ', ' ge '),
                                               (' < ', ' lt '),
                                               (' <= ', ' le '),
                                               (' + ', ' add '),
                                               (' - ', ' sub '),
                                               (' * ', ' mul '),
                                               (' / ', ' div ') );
var
  iFor: Integer;
begin
  Result := AParams;
  for iFor := Low(OPERATORMAP) to High(OPERATORMAP) do
    Result := StringReplace(Result, OPERATORMAP[iFor, 0], OPERATORMAP[iFor, 1], [rfReplaceAll]);
end;

end.

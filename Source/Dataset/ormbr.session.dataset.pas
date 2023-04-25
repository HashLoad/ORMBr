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

unit ormbr.session.dataset;

interface

uses
  DB,
  Rtti,
  TypInfo,
  Classes,
  Variants,
  SysUtils,
  Generics.Collections,
  /// orm
  ormbr.command.executor,
  ormbr.session.abstract,
  ormbr.dataset.base.adapter,
  dbcbr.mapping.classes,
  dbebr.factory.interfaces;

type
  // M - Sessão DataSet
  TSessionDataSet<M: class, constructor> = class(TSessionAbstract<M>)
  private
    FOwner: TDataSetBaseAdapter<M>;
    procedure PopularDataSet(const ADBResultSet: IDBResultSet);
  protected
    FConnection: IDBConnection;
  public
    constructor Create(const AOwner: TDataSetBaseAdapter<M>;
      const AConnection: IDBConnection; const APageSize: Integer = -1); overload;
    destructor Destroy; override;
    procedure OpenID(const AID: Variant); override;
    procedure OpenSQL(const ASQL: string); override;
    procedure OpenWhere(const AWhere: string; const AOrderBy: string = ''); override;
    procedure NextPacket; override;
    procedure RefreshRecord(const AColumns: TParams); override;
    procedure RefreshRecordWhere(const AWhere: String); override;
//    procedure NextPacketList(const AObjectList: TObjectList<M>); overload; override;
//    function NextPacketList: TObjectList<M>; overload; override;
//    function NextPacketList(const APageSize, APageNext: Integer): TObjectList<M>; overload; override;
//    function NextPacketList(const AWhere, AOrderBy: String; const APageSize, APageNext: Integer): TObjectList<M>; overload; override;
    function SelectAssociation(const AObject: TObject): String; override;
  end;

implementation

uses
  ormbr.bind;

{ TSessionDataSet<M> }

constructor TSessionDataSet<M>.Create(const AOwner: TDataSetBaseAdapter<M>;
  const AConnection: IDBConnection; const APageSize: Integer);
begin
  inherited Create(APageSize);
  FOwner := AOwner;
  FConnection := AConnection;
  FCommandExecutor := TSQLCommandExecutor<M>.Create(Self, AConnection, APageSize);
end;

destructor TSessionDataSet<M>.Destroy;
begin
  FCommandExecutor.Free;
  inherited;
end;

function TSessionDataSet<M>.SelectAssociation(const AObject: TObject): String;
begin
  inherited;
  Result := FCommandExecutor.SelectInternalAssociation(AObject);
end;

procedure TSessionDataSet<M>.OpenID(const AID: Variant);
var
  LDBResultSet: IDBResultSet;
begin
  inherited;
  LDBResultSet := FCommandExecutor.SelectInternalID(AID);
  // Popula o DataSet em memória com os registros retornardos no comando SQL
  PopularDataSet(LDBResultSet);
end;

procedure TSessionDataSet<M>.OpenSQL(const ASQL: string);
var
  LDBResultSet: IDBResultSet;
begin
  inherited;
  if ASQL = '' then
    LDBResultSet := FCommandExecutor.SelectInternalAll
  else
    LDBResultSet := FCommandExecutor.SelectInternal(ASQL);
  PopularDataSet(LDBResultSet);
end;

procedure TSessionDataSet<M>.OpenWhere(const AWhere: string;
  const AOrderBy: string);
begin
  inherited;
  OpenSQL(FCommandExecutor.SelectInternalWhere(AWhere, AOrderBy));
end;

procedure TSessionDataSet<M>.RefreshRecord(const AColumns: TParams);
var
  LDBResultSet: IDBResultSet;
  LWhere: String;
  LFor: Integer;
begin
  inherited;
  LWhere := '';
  for LFor := 0 to AColumns.Count -1 do
  begin
    LWhere := LWhere + AColumns[LFor].Name + '=' + AColumns[LFor].AsString;
    if LFor < AColumns.Count -1 then
      LWhere := LWhere + ' AND ';
  end;
  LDBResultSet := FCommandExecutor.SelectInternal(FCommandExecutor.SelectInternalWhere(LWhere, ''));
  // Atualiza dados no DataSet
  while LDBResultSet.NotEof do
  begin
    FOwner.FOrmDataSet.Edit;
    Bind.SetFieldToField(LDBResultSet, FOwner.FOrmDataSet);
    FOwner.FOrmDataSet.Post;
  end;
end;

procedure TSessionDataSet<M>.RefreshRecordWhere(const AWhere: String);
var
  LDBResultSet: IDBResultSet;
begin
  inherited;
  LDBResultSet := FCommandExecutor.SelectInternal(FCommandExecutor.SelectInternalWhere(AWhere, ''));
  // Atualiza dados no DataSet
  while LDBResultSet.NotEof do
  begin
    FOwner.FOrmDataSet.Edit;
    Bind.SetFieldToField(LDBResultSet, FOwner.FOrmDataSet);
    FOwner.FOrmDataSet.Post;
  end;
end;

procedure TSessionDataSet<M>.NextPacket;
var
  LDBResultSet: IDBResultSet;
begin
  inherited;
  LDBResultSet := FCommandExecutor.NextPacket;
  if LDBResultSet.RecordCount > 0 then
    // Popula o DataSet em memória com os registros retornardos no comando SQL
    PopularDataSet(LDBResultSet)
  else
    FFetchingRecords := True;
end;

//procedure TSessionDataSet<M>.NextPacketList(const AObjectList: TObjectList<M>);
//begin
//  inherited;
//  if FFetchingRecords then
//    Exit;
//
//  FPageNext := FPageNext + FPageSize;
//  if FFindWhereUsed then
//    FCommandExecutor.NextPacketList(AObjectList, FWhere, FOrderBy, FPageSize, FPageNext)
//  else
//    FCommandExecutor.NextPacketList(AObjectList, FPageSize, FPageNext);
//  /// <summary>
//  ///    if AObjectList <> nil then
//  ///      if AObjectList.RecordCount = 0 then
//  ///        FFetchingRecords := True;
//  ///  Esse código para definir a tag FFetchingRecords, está sendo feito no
//  ///  método NextPacketList() dentro do FCommandExecutor.
//  /// </summary>
//end;

//function TSessionDataSet<M>.NextPacketList: TObjectList<M>;
//begin
//  inherited;
//  Result := nil;
//  if FFetchingRecords then
//    Exit;
//
//  FPageNext := FPageNext + FPageSize;
//  if FFindWhereUsed then
//    Result := FCommandExecutor.NextPacketList(FWhere, FOrderBy, FPageSize, FPageNext)
//  else
//    Result := FCommandExecutor.NextPacketList(FPageSize, FPageNext);
//
//  if Result = nil then
//    Exit;
//  if Result.Count > 0 then
//    Exit;
//
//  FFetchingRecords := True;
//end;

//function TSessionDataSet<M>.NextPacketList(const APageSize,
//  APageNext: Integer): TObjectList<M>;
//begin
//  inherited;
//  Result := nil;
//  if FFetchingRecords then
//    Exit;
//
//  Result := FCommandExecutor.NextPacketList(APageSize, APageNext);
//  if Result = nil then
//    Exit;
//  if Result.Count > 0 then
//    Exit;
//
//  FFetchingRecords := True;
//end;

//function TSessionDataSet<M>.NextPacketList(const AWhere, AOrderBy: String;
//  const APageSize, APageNext: Integer): TObjectList<M>;
//begin
//  inherited;
//  Result := nil;
//  if FFetchingRecords then
//    Exit;
//
//  Result := FCommandExecutor.NextPacketList(AWhere, AOrderBy, APageSize, APageNext);
//  if Result = nil then
//    Exit;
//  if Result.Count > 0 then
//    Exit;
//
//  FFetchingRecords := True;
//end;

procedure TSessionDataSet<M>.PopularDataSet(const ADBResultSet: IDBResultSet);
begin
//  FOrmDataSet.Locate(KeyFiels, KeyValues, Options);
//  { TODO -oISAQUE : Procurar forma de verificar se o registro não já está em memória
//  pela chave primaria }
  try
    while ADBResultSet.NotEof do
    begin
       FOwner.FOrmDataSet.Append;
       Bind.SetFieldToField(ADBResultSet, FOwner.FOrmDataSet);
       FOwner.FOrmDataSet.Fields[0].AsInteger := -1;
       FOwner.FOrmDataSet.Post;
    end;
  finally
    // Aqui o DataSet(FOrmDataSet) dessa sessão recebe os dados do select executado,
    // em seguinda o Dataset interno ao driver selecionado é fechado,
    // limpando assim os dados dele da memória.
    ADBResultSet.Close;
  end;
end;

end.

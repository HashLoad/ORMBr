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
}

unit ormbr.objectset.adapter;

interface

uses
  Rtti,
  TypInfo,
  Classes,
  Variants,
  SysUtils,
  Generics.Collections,
  // ORMBr
  ormbr.objectset.base.adapter,
  ormbr.objects.helper,
  dbebr.factory.interfaces,
  dbcbr.mapping.classes,
  dbcbr.mapping.popular,
  dbcbr.types.mapping;

type
  TObjectSetAdapter<M: class, constructor> = class(TObjectSetBaseAdapter<M>)
  private
    FConnection: IDBConnection;
  public
    constructor Create(const AConnection: IDBConnection;
      const APageSize: Integer = -1); overload;
    destructor Destroy; override;
    function Find: TObjectList<M>; overload; override;
    function Find(const AID: Int64): M; overload; override;
    function Find(const AID: string): M; overload; override;
    function FindWhere(const AWhere: string;
      const AOrderBy: string = ''): TObjectList<M>; overload; override;
    procedure Insert(const AObject: M); override;
    procedure Update(const AObject: M); override;
    procedure Delete(const AObject: M); override;
  end;

implementation

uses
  ormbr.session.objectset,
  ormbr.core.consts,
  dbcbr.mapping.explorer;

{ TObjectSetAdapter<M> }

constructor TObjectSetAdapter<M>.Create(const AConnection: IDBConnection;
  const APageSize: Integer);
begin
  inherited Create;
  FConnection := AConnection;
  FSession := TSessionObjectSet<M>.Create(AConnection, APageSize);
end;

destructor TObjectSetAdapter<M>.Destroy;
begin
  FSession.Free;
  inherited;
end;

procedure TObjectSetAdapter<M>.Delete(const AObject: M);
var
  LInTransaction: Boolean;
  LIsConnected: Boolean;
begin
  inherited;
  // Controle de transação externa, controlada pelo desenvolvedor
  LInTransaction := FConnection.InTransaction;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    if not LInTransaction then
      FConnection.StartTransaction;
    try
      // Executa comando delete em cascade
      CascadeActionsExecute(AObject, TCascadeAction.CascadeDelete);
      // Executa comando delete master
      FSession.Delete(AObject);
      ///
      if not LInTransaction then
        FConnection.Commit;
    except
      on E: Exception do
      begin
        if not LInTransaction then
          FConnection.Rollback;
        raise Exception.Create(E.Message);
      end;
    end;
  finally
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

function TObjectSetAdapter<M>.FindWhere(const AWhere, AOrderBy: string): TObjectList<M>;
var
  LIsConnected: Boolean;
begin
  inherited;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    Result := FSession.FindWhere(AWhere, AOrderBy);
  finally
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

function TObjectSetAdapter<M>.Find(const AID: Int64): M;
var
  LIsConnected: Boolean;
begin
  inherited;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    Result := FSession.Find(AID);
  finally
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

function TObjectSetAdapter<M>.Find: TObjectList<M>;
var
  LIsConnected: Boolean;
begin
  inherited;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    Result := FSession.Find;
  finally
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

procedure TObjectSetAdapter<M>.Insert(const AObject: M);
var
  LPrimaryKey: TPrimaryKeyColumnsMapping;
  LColumn: TColumnMapping;
  LInTransaction: Boolean;
  LIsConnected: Boolean;
begin
  inherited;
  // Controle de transação externa, controlada pelo desenvolvedor
  LInTransaction := FConnection.InTransaction;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    if not LInTransaction then
      FConnection.StartTransaction;
    try
      FSession.Insert(AObject);
      if FSession.ExistSequence then
      begin
        LPrimaryKey := TMappingExplorer
                           .GetMappingPrimaryKeyColumns(AObject.ClassType);
        if LPrimaryKey = nil then
          raise Exception.Create(cMESSAGEPKNOTFOUND);

        for LColumn in LPrimaryKey.Columns do
          SetAutoIncValueChilds(AObject, LColumn);
      end;
      // Executa comando insert em cascade
      CascadeActionsExecute(AObject, TCascadeAction.CascadeInsert);
      //
      if not LInTransaction then
        FConnection.Commit;
    except
      on E: Exception do
      begin
        if not LInTransaction then
          FConnection.Rollback;
        raise Exception.Create(E.Message);
      end;
    end;
  finally
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

procedure TObjectSetAdapter<M>.Update(const AObject: M);
var
  LRttiType: TRttiType;
  LProperty: TRttiProperty;
  LObjectKey: TObject;
  LKey: string;
  LInTransaction: Boolean;
  LIsConnected: Boolean;
begin
  inherited;
  // Controle de transação externa, controlada pelo desenvolvedor
  LInTransaction := FConnection.InTransaction;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    if not LInTransaction then
      FConnection.StartTransaction;
    try
      // Executa comando update em cascade
      CascadeActionsExecute(AObject, TCascadeAction.CascadeUpdate);
      // Gera a lista com as propriedades que foram alteradas
      if TObject(AObject).GetType(LRttiType) then
      begin
        LKey := GenerateKey(AObject);
        if FObjectState.TryGetValue(LKey, LObjectKey) then
        begin
          FSession.ModifyFieldsCompare(LKey, LObjectKey, AObject);
          FSession.Update(AObject, LKey);
          FObjectState.Remove(LKey);
          FObjectState.TrimExcess;
        end;
        // Remove o item excluído em Update Mestre-Detalhe
        for LObjectKey in FObjectState.Values do
          FSession.Delete(LObjectKey);
      end;
      if not LInTransaction then
        FConnection.Commit;
    except
      on E: Exception do
      begin
        if not LInTransaction then
          FConnection.Rollback;
        raise Exception.Create(E.Message);
      end;
    end;
  finally
    if not LIsConnected then
      FConnection.Disconnect;
    FObjectState.Clear;
    // Após executar o comando SQL Update, limpa a lista de campos alterados.
    FSession.ModifiedFields.Clear;
    FSession.ModifiedFields.TrimExcess;
    FSession.DeleteList.Clear;
    FSession.DeleteList.TrimExcess;
  end;
end;

function TObjectSetAdapter<M>.Find(const AID: string): M;
var
  LIsConnected: Boolean;
begin
  inherited;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    Result := FSession.Find(AID);
  finally
    if not LIsConnected then
      FConnection.Disconnect;
  end;
end;

end.

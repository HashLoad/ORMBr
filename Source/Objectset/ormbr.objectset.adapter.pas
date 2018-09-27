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

unit ormbr.objectset.adapter;

interface

uses
  Rtti,
  TypInfo,
  Classes,
  Variants,
  SysUtils,
  Generics.Collections,
  /// ORMBr
  ormbr.objectset.base.adapter,
  ormbr.factory.interfaces,
  ormbr.mapping.classes,
  ormbr.types.mapping,
  ormbr.objects.helper;

type
  /// <summary>
  /// M - Object M
  /// </summary>
  TObjectSetAdapter<M: class, constructor> = class(TObjectSetBaseAdapter<M>)
  private
    FConnection: IDBConnection;
  public
    constructor Create(const AConnection: IDBConnection; const APageSize: Integer = -1); overload;
    destructor Destroy; override;
    function Find: TObjectList<M>; overload; override;
    function Find(const AID: Integer): M; overload; override;
    function Find(const AID: string): M; overload; override;
    function FindWhere(const AWhere: string; const AOrderBy: string = ''): TObjectList<M>; overload; override;
    procedure Insert(const AObject: M); override;
    procedure Update(const AObject: M); override;
    procedure Delete(const AObject: M); override;
  end;

implementation

uses
  ormbr.session.objectset;

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
  /// <summary>
  /// Controle de transação externa, controlada pelo desenvolvedor
  /// </summary>
  LInTransaction := FConnection.InTransaction;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    if not LInTransaction then
      FConnection.StartTransaction;
    try
      /// <summary>
      /// Executa comando delete em cascade
      /// </summary>
      CascadeActionsExecute(AObject, CascadeDelete);
      /// <summary>
      /// Executa comando delete master
      /// </summary>
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

function TObjectSetAdapter<M>.Find(const AID: Integer): M;
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
  LColumn: TColumnMapping;
  LInTransaction: Boolean;
  LIsConnected: Boolean;
begin
  inherited;
  /// <summary>
  /// Controle de transação externa, controlada pelo desenvolvedor
  /// </summary>
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
        for LColumn in AObject.GetPrimaryKey do
          SetAutoIncValueChilds(AObject, LColumn);
      end;
      /// <summary> Executa comando insert em cascade </summary>
      CascadeActionsExecute(AObject, CascadeInsert);
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

procedure TObjectSetAdapter<M>.Update(const AObject: M);
var
  LRttiType: TRttiType;
  LProperty: TRttiProperty;
  LObject: TObject;
  LKey: string;
  LInTransaction: Boolean;
  LIsConnected: Boolean;
begin
  inherited;
  /// <summary>
  /// Controle de transação externa, controlada pelo desenvolvedor
  /// </summary>
  LInTransaction := FConnection.InTransaction;
  LIsConnected := FConnection.IsConnected;
  if not LIsConnected then
    FConnection.Connect;
  try
    if not LInTransaction then
      FConnection.StartTransaction;
    try
      /// <summary> Executa comando update em cascade </summary>
      CascadeActionsExecute(AObject, CascadeUpdate);
      /// <summary> Gera a lista com as propriedades que foram alteradas </summary>
      if TObject(AObject).GetType(LRttiType) then
      begin
        LKey := GenerateKey(AObject);
        if FObjectState.ContainsKey(LKey) then
        begin
          LObject := FObjectState.Items[LKey];
          FSession.ModifyFieldsCompare(LKey, AObject, LObject);
          FSession.Update(AObject, LKey);
          FObjectState.Remove(LKey);
          FObjectState.TrimExcess;
        end;
        /// <summary>
        /// Remove o item excluído em Update Mestre-Detalhe
        /// </summary>
        for LObject in FObjectState.Values do
          FSession.Delete(LObject);
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
    /// <summary>
    /// Após executar o comando SQL Update, limpa a lista de campos alterados.
    /// </summary>
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

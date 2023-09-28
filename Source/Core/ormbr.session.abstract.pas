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

{$INCLUDE ..\ormbr.inc}

unit ormbr.session.abstract;

interface

uses
  DB,
  Rtti,
  TypInfo,
  SysUtils,
  Generics.Collections,
  /// ORMBr
  ormbr.bind,
  ormbr.core.consts,
  ormbr.rtti.helper,
  ormbr.types.blob,
  dbcbr.mapping.popular,
  dbcbr.mapping.attributes,
  dbebr.factory.interfaces,
  ormbr.command.executor.abstract;

type
  TSessionAbstract<M: class, constructor> = class abstract
  protected
    FPageSize: Integer;
    FPageNext: Integer;
    FDeleteList: TObjectList<M>;
    FResultParams: TParams;
    FFindWhereUsed: Boolean;
    FFindWhereRefreshUsed: Boolean;
    FFetchingRecords: Boolean;
    FWhere: String;
    FOrderBy: String;
    FModifiedFields: TDictionary<string, TDictionary<string, string>>;
    FCommandExecutor: TSQLCommandExecutorAbstract<M>;
    function PopularObjectSet(const ADBResultSet: IDBResultSet): TObjectList<M>;
  public
    constructor Create(const APageSize: Integer = -1); overload; virtual;
    destructor Destroy; override;
    function ExistSequence: Boolean; virtual;
    function ModifiedFields: TDictionary<string, TDictionary<string, string>>; virtual;
    // ObjectSet
    procedure Insert(const AObject: M); overload; virtual;
    procedure Insert(const AObjectList: TObjectList<M>); overload; virtual; abstract;
    procedure Update(const AObject: M; const AKey: string); overload; virtual;
    procedure Update(const AObjectList: TObjectList<M>); overload; virtual; abstract;
    procedure Delete(const AObject: M); overload; virtual;
    procedure Delete(const AID: Integer); overload; virtual; abstract;
    procedure LoadLazy(const AOwner, AObject: TObject); virtual;
    procedure NextPacketList(const AObjectList: TObjectList<M>); overload; virtual;
    function NextPacketList: TObjectList<M>; overload; virtual;
    function NextPacketList(const APageSize,
      APageNext: Integer): TObjectList<M>; overload; virtual;
    function NextPacketList(const AWhere, AOrderBy: String;
      const APageSize, APageNext: Integer): TObjectList<M>; overload; virtual;
    // DataSet
    procedure Open; virtual;
    procedure OpenID(const AID: TValue); virtual;
    procedure OpenSQL(const ASQL: string); virtual;
    procedure OpenWhere(const AWhere: string; const AOrderBy: string = ''); virtual;
    procedure NextPacket; overload; virtual;
    procedure RefreshRecord(const AColumns: TParams); virtual;
    procedure RefreshRecordWhere(const AWhere: String); virtual;
    function SelectAssociation(const AObject: TObject): String; virtual;
    function ResultParams: TParams;
    // DataSet e ObjectSet
    procedure ModifyFieldsCompare(const AKey: string; const AObjectSource,
      AObjectUpdate: TObject); virtual;
    function Find: TObjectList<M>; overload; virtual;
    function Find(const AID: Int64): M; overload; virtual;
    function Find(const AID: string): M; overload; virtual;
    {$IFDEF DRIVERRESTFUL}
    function Find(const AMethodName: String;
      const AParams: array of string): TObjectList<M>; overload; virtual; abstract;
    {$ENDIF}
    function FindWhere(const AWhere: string;
      const AOrderBy: string): TObjectList<M>; virtual;
    function DeleteList: TObjectList<M>; virtual;
    //
    property FetchingRecords: Boolean read FFetchingRecords write FFetchingRecords;
  end;

implementation

uses
  ormbr.objects.helper,
  dbcbr.mapping.explorer,
  dbcbr.mapping.classes;

{ TSessionAbstract<M> }

constructor TSessionAbstract<M>.Create(const APageSize: Integer = -1);
begin
  FPageSize := APageSize;
  FModifiedFields := TObjectDictionary<string, TDictionary<string, string>>.Create([doOwnsValues]);
  FDeleteList := TObjectList<M>.Create;
  FResultParams := TParams.Create;
  FFetchingRecords := False;
  // Inicia uma lista interna para gerenciar campos alterados
  FModifiedFields.Clear;
  FModifiedFields.TrimExcess;
  FModifiedFields.Add(M.ClassName, TDictionary<string, string>.Create);
end;

destructor TSessionAbstract<M>.Destroy;
begin
  FDeleteList.Clear;
  FDeleteList.Free;
  FModifiedFields.Clear;
  FModifiedFields.Free;
  FResultParams.Clear;
  FResultParams.Free;
  inherited;
end;

function TSessionAbstract<M>.ModifiedFields: TDictionary<string, TDictionary<string, string>>;
begin
  Result := FModifiedFields;
end;

procedure TSessionAbstract<M>.Delete(const AObject: M);
begin
  FCommandExecutor.DeleteInternal(AObject);
end;

function TSessionAbstract<M>.DeleteList: TObjectList<M>;
begin
  Result := FDeleteList;
end;

function TSessionAbstract<M>.ExistSequence: Boolean;
begin
  Result := FCommandExecutor.ExistSequence;
end;

function TSessionAbstract<M>.Find(const AID: string): M;
begin
  FFindWhereUsed := False;
  FFetchingRecords := False;
  Result := FCommandExecutor.Find(AID);
end;

function TSessionAbstract<M>.FindWhere(const AWhere,
  AOrderBy: string): TObjectList<M>;
var
  LDBResultSet: IDBResultSet;
begin
  FFindWhereUsed := True;
  FFetchingRecords := False;
  FWhere := AWhere;
  FOrderBy := AOrderBy;
  if FPageSize > -1 then
  begin
    LDBResultSet := FCommandExecutor.NextPacketList(FWhere, FOrderBy, FPageSize, FPageNext);
    Result := PopularObjectSet(LDBResultSet);
    Exit;
  end;
  LDBResultSet := FCommandExecutor.FindWhere(FWhere, FOrderBy);
  Result := PopularObjectSet(LDBResultSet);
end;

function TSessionAbstract<M>.Find(const AID: Int64): M;
begin
  FFindWhereUsed := False;
  FFetchingRecords := False;
  Result := FCommandExecutor.Find(AID);
end;

function TSessionAbstract<M>.Find: TObjectList<M>;
var
  LDBResultSet: IDBResultSet;
  LObject: M;
begin
  FFindWhereUsed := False;
  FFetchingRecords := False;
  LDBResultSet := FCommandExecutor.Find;
  Result := PopularObjectSet(LDBResultSet)
end;

procedure TSessionAbstract<M>.Insert(const AObject: M);
begin
  FCommandExecutor.InsertInternal(AObject);
end;

procedure TSessionAbstract<M>.ModifyFieldsCompare(const AKey: string;
  const AObjectSource, AObjectUpdate: TObject);
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
  LProperty: TRttiProperty;
begin
  LColumns := TMappingExplorer.GetMappingColumn(AObjectSource.ClassType);
  for LColumn in LColumns do
  begin
    LProperty := LColumn.ColumnProperty;
    if LProperty.IsVirtualData then
      Continue;
    if LProperty.IsNoUpdate then
      Continue;
    if LProperty.PropertyType.TypeKind in cPROPERTYTYPES_1 then
      Continue;
    if not FModifiedFields.ContainsKey(AKey) then
      FModifiedFields.Add(AKey, TDictionary<string, string>.Create);
    // Se o tipo da property for tkRecord provavelmente tem Nullable nela
    // Se não for tkRecord entra no ELSE e pega o valor de forma direta
    if LProperty.PropertyType.TypeKind in [tkRecord] then // Nullable ou TBlob
    begin
      if LProperty.IsBlob then
      begin
        if LProperty.GetValue(AObjectSource).AsType<TBlob>.ToSize <>
           LProperty.GetValue(AObjectUpdate).AsType<TBlob>.ToSize then
        begin
          FModifiedFields.Items[AKey].Add(LProperty.Name, LColumn.ColumnName);
        end;
      end
      else
      begin
        if LProperty.GetNullableValue(AObjectSource).AsType<Variant> <>
           LProperty.GetNullableValue(AObjectUpdate).AsType<Variant> then
        begin
          FModifiedFields.Items[AKey].Add(LProperty.Name, LColumn.ColumnName);
        end;
      end;
    end
    else
    begin
      if LProperty.GetValue(AObjectSource).AsType<Variant> <>
         LProperty.GetValue(AObjectUpdate).AsType<Variant> then
      begin
        FModifiedFields.Items[AKey].Add(LProperty.Name, LColumn.ColumnName);
      end;
    end;
  end;
end;

procedure TSessionAbstract<M>.NextPacket;
begin

end;

procedure TSessionAbstract<M>.NextPacketList(const AObjectList: TObjectList<M>);
begin
  if FFetchingRecords then
    Exit;
  FPageNext := FPageNext + FPageSize;
  if FFindWhereUsed then
    FCommandExecutor.NextPacketList(AObjectList, FWhere, FOrderBy, FPageSize, FPageNext)
  else
    FCommandExecutor.NextPacketList(AObjectList, FPageSize, FPageNext);
end;

function TSessionAbstract<M>.NextPacketList: TObjectList<M>;
var
  LDBResultSet: IDBResultSet;
begin
  inherited;
  Result := nil;
  if FFetchingRecords then
    Exit;
  FPageNext := FPageNext + FPageSize;
  if FFindWhereUsed then
    LDBResultSet := FCommandExecutor.NextPacketList(FWhere, FOrderBy, FPageSize, FPageNext)
  else
    LDBResultSet := FCommandExecutor.NextPacketList(FPageSize, FPageNext);
  Result := PopularObjectSet(LDBResultSet);
end;

function TSessionAbstract<M>.NextPacketList(const APageSize,
  APageNext: Integer): TObjectList<M>;
var
  LDBResultSet: IDBResultSet;
begin
  inherited;
  Result := nil;
  if FFetchingRecords then
    Exit;
  LDBResultSet := FCommandExecutor.NextPacketList(APageSize, APageNext);
  Result := PopularObjectSet(LDBResultSet);
end;

function TSessionAbstract<M>.NextPacketList(const AWhere, AOrderBy: String;
  const APageSize, APageNext: Integer): TObjectList<M>;
var
 LDBResultSet: IDBResultSet;
begin
  inherited;
  Result := nil;
  if FFetchingRecords then
    Exit;
  LDBResultSet := FCommandExecutor.NextPacketList(AWhere, AOrderBy, APageSize, APageNext);
  Result := PopularObjectSet(LDBResultSet);
end;

procedure TSessionAbstract<M>.Open;
begin
  FFetchingRecords := False;
end;

procedure TSessionAbstract<M>.OpenID(const AID: TValue);
begin
  FFetchingRecords := False;
end;

procedure TSessionAbstract<M>.OpenSQL(const ASQL: string);
begin
  FFetchingRecords := False;
end;

procedure TSessionAbstract<M>.OpenWhere(const AWhere, AOrderBy: string);
begin
  FFetchingRecords := False;
end;

function TSessionAbstract<M>.PopularObjectSet(
  const ADBResultSet: IDBResultSet): TObjectList<M>;
var
  LObjectList: TObjectList<M>;
begin
  LObjectList := TObjectList<M>.Create;
  Result := LObjectList;
  try
    while ADBResultSet.NotEof do
    begin
      Result.Add(M.Create);
      Bind.SetFieldToProperty(ADBResultSet, TObject(Result.Last));
      // Alimenta registros das associações existentes 1:1 ou 1:N
      FCommandExecutor.FillAssociation(Result.Last);
    end;
    if Result.Count > 0 then
      Exit;
    FFetchingRecords := True;
  finally
    ADBResultSet.Close;
  end;
end;

procedure TSessionAbstract<M>.RefreshRecord(const AColumns: TParams);
begin

end;

procedure TSessionAbstract<M>.RefreshRecordWhere(const AWhere: String);
begin

end;

function TSessionAbstract<M>.ResultParams: TParams;
begin
  Result := FResultParams;
end;

function TSessionAbstract<M>.SelectAssociation(const AObject: TObject): String;
begin
  Result := ''
end;

procedure TSessionAbstract<M>.Update(const AObject: M; const AKey: string);
begin
  FCommandExecutor.UpdateInternal(AObject, FModifiedFields.Items[AKey]);
end;

procedure TSessionAbstract<M>.LoadLazy(const AOwner, AObject: TObject);
begin

end;

end.

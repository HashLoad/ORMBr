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

unit ormbr.session.abstract;

interface

uses
  DB,
  Rtti,
  TypInfo,
  SysUtils,
  Generics.Collections,
  /// ORMBr
  ormbr.core.consts,
  ormbr.rtti.helper,
  ormbr.types.blob,
  ormbr.mapping.attributes,
  ormbr.objects.manager.abstract;

type
  // M - Sessão Abstract
  TSessionAbstract<M: class, constructor> = class abstract
  protected
    {$IFDEF USEBINDSOURCE}
    FOnPropertyEvent: TProc<TRttiProperty, String>;
    FOnUpdateEvent: TProc<TObject>;
    {$ENDIF}
    FPageSize: Integer;
    FPageNext: Integer;
    FModifiedFields: TDictionary<string, TDictionary<string, string>>;
    FDeleteList: TObjectList<M>;
    FManager: TObjectManagerAbstract<M>;
    FResultParams: TParams;
    FFindWhereUsed: Boolean;
    FFindWhereRefreshUsed: Boolean;
    FFetchingRecords: Boolean;
    FWhere: String;
    FOrderBy: String;
    {$IFDEF DRIVERRESTFUL}
    function Find(const AMethodName: String;
      const AParams: array of string): TObjectList<M>; overload; virtual; abstract;
    {$ENDIF}
  public
    constructor Create(const APageSize: Integer = -1); overload; virtual;
    destructor Destroy; override;
    function ExistSequence: Boolean; virtual;
    function ModifiedFields: TDictionary<string, TDictionary<string, string>>; virtual;
    /// <summary>
    ///   ObjectSet
    /// </summary>
    procedure Insert(const AObject: M); overload; virtual;
    procedure Insert(const AObjectList: TObjectList<M>); overload; virtual; abstract;
    procedure Update(const AObject: M; const AKey: string); overload; virtual;
    procedure Update(const AObjectList: TObjectList<M>); overload; virtual; abstract;
    procedure Delete(const AObject: M); overload; virtual;
    procedure Delete(const AID: Integer); overload; virtual; abstract;
    procedure LoadLazy(const AOwner, AObject: TObject); virtual;
    procedure NextPacketList(const AObjectList: TObjectList<M>); overload; virtual; abstract;
    function NextPacketList: TObjectList<M>; overload; virtual; abstract;
    function NextPacketList(const APageSize,
      APageNext: Integer): TObjectList<M>; overload; virtual; abstract;
    function NextPacketList(const AWhere, AOrderBy: String;
      const APageSize, APageNext: Integer): TObjectList<M>; overload; virtual; abstract;
    /// <summary>
    ///   DataSet
    /// </summary>
    procedure Open; virtual;
    procedure OpenID(const AID: Variant); virtual;
    procedure OpenSQL(const ASQL: string); virtual;
    procedure OpenWhere(const AWhere: string; const AOrderBy: string = ''); virtual;
    procedure NextPacket; overload; virtual;
    procedure RefreshRecord(const AColumns: TParams); virtual;
    function SelectAssociation(const AObject: TObject): String; virtual;
    function ResultParams: TParams;
    /// <summary>
    ///   DataSet e ObjectSet
    /// </summary>
    procedure ModifyFieldsCompare(const AKey: string; const AObjectSource,
      AObjectUpdate: TObject); virtual;
    function Find: TObjectList<M>; overload; virtual;
    function Find(const AID: Integer): M; overload; virtual;
    function Find(const AID: string): M; overload; virtual;
    function FindWhere(const AWhere: string;
      const AOrderBy: string): TObjectList<M>; virtual;
    function DeleteList: TObjectList<M>; virtual;

    property FetchingRecords: Boolean read FFetchingRecords write FFetchingRecords;
    {$IFDEF USEBINDSOURCE}
    property OnPropertyEvent: TProc<TRttiProperty, String> read FOnPropertyEvent
                                                          write FOnPropertyEvent;
    property OnUpdateEvent: TProc<TObject> read FOnUpdateEvent
                                          write FOnUpdateEvent;
    {$ENDIF}
  end;

implementation

uses
  ormbr.objects.helper,
  ormbr.mapping.explorer,
  ormbr.mapping.classes;

{ TSessionAbstract<M> }

constructor TSessionAbstract<M>.Create(const APageSize: Integer = -1);
begin
  FPageSize := APageSize;
  FModifiedFields := TObjectDictionary<string, TDictionary<string, string>>.Create([doOwnsValues]);
  FDeleteList := TObjectList<M>.Create;
  FResultParams := TParams.Create;
  FFetchingRecords := False;
  /// <summary>
  ///   Inicia uma lista interna para gerenciar campos alterados
  /// </summary>
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
  FManager.DeleteInternal(AObject);
end;

function TSessionAbstract<M>.DeleteList: TObjectList<M>;
begin
  Result := FDeleteList;
end;

function TSessionAbstract<M>.ExistSequence: Boolean;
begin
  Result := FManager.ExistSequence;
end;

function TSessionAbstract<M>.Find(const AID: string): M;
begin
  FFindWhereUsed := False;
  FFetchingRecords := False;
  Result := FManager.Find(AID);
end;

function TSessionAbstract<M>.FindWhere(const AWhere,
  AOrderBy: string): TObjectList<M>;
begin
  FFindWhereUsed := True;
  FFetchingRecords := False;
  FWhere := AWhere;
  FOrderBy := AOrderBy;
  if FPageSize > -1 then
  begin
    Result := NextPacketList(FWhere, FOrderBy, FPageSize, FPageNext);
    Exit;
  end;
  Result := FManager.FindWhere(FWhere, FOrderBy);
end;

function TSessionAbstract<M>.Find(const AID: Integer): M;
begin
  FFindWhereUsed := False;
  FFetchingRecords := False;
  Result := FManager.Find(AID);
end;

function TSessionAbstract<M>.Find: TObjectList<M>;
begin
  FFindWhereUsed := False;
  FFetchingRecords := False;
  Result := FManager.Find;
end;

procedure TSessionAbstract<M>.Insert(const AObject: M);
begin
  FManager.InsertInternal(AObject);
end;

procedure TSessionAbstract<M>.ModifyFieldsCompare(const AKey: string;
  const AObjectSource, AObjectUpdate: TObject);
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
  LProperty: TRttiProperty;
begin
  LColumns := TMappingExplorer.GetInstance.GetMappingColumn(AObjectSource.ClassType);
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
          // Bind object property in control
          {$IFDEF USEBINDSOURCE}
//            if Assigned(FOnPropertyEvent) then
//              OnPropertyEvent(LProperty, AObjectUpdate.ClassName);
          {$ENDIF}
        end;
      end
      else
      begin
        if LProperty.GetNullableValue(AObjectSource).AsType<Variant> <>
           LProperty.GetNullableValue(AObjectUpdate).AsType<Variant> then
        begin
          FModifiedFields.Items[AKey].Add(LProperty.Name, LColumn.ColumnName);
          // Bind object property in control
          {$IFDEF USEBINDSOURCE}
//            if Assigned(FOnPropertyEvent) then
//              OnPropertyEvent(LProperty, AObjectUpdate.ClassName);
          {$ENDIF}
        end;
      end;
    end
    else
    begin
      if LProperty.GetValue(AObjectSource).AsType<Variant> <>
         LProperty.GetValue(AObjectUpdate).AsType<Variant> then
      begin
        FModifiedFields.Items[AKey].Add(LProperty.Name, LColumn.ColumnName);
        // Bind object property in control
        {$IFDEF USEBINDSOURCE}
//          if Assigned(FOnPropertyEvent) then
//            OnPropertyEvent(LProperty, AObjectUpdate.ClassName);
        {$ENDIF}
      end;
    end;
  end;
end;

procedure TSessionAbstract<M>.NextPacket;
begin

end;

procedure TSessionAbstract<M>.Open;
begin
  FFetchingRecords := False;
end;

procedure TSessionAbstract<M>.OpenID(const AID: Variant);
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

procedure TSessionAbstract<M>.RefreshRecord(const AColumns: TParams);
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
  FManager.UpdateInternal(AObject, FModifiedFields.Items[AKey]);
end;

procedure TSessionAbstract<M>.LoadLazy(const AOwner, AObject: TObject);
begin

end;

end.

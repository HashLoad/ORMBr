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

{
  @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

{$INCLUDE ..\ormbr.inc}

unit ormbr.manager.dataset;

interface

uses
  DB,
  Rtti,
  SysUtils,
  Generics.Collections,
  {$IFDEF USEFDMEMTABLE}
    FireDAC.Comp.Client,
    {$IFDEF DRIVERRESTFUL}
      ormbr.restdataset.fdmemtable
    {$ELSE}
      ormbr.dataset.fdmemtable
    {$ENDIF},
  {$ENDIF}

  {$IFDEF USECLIENTDATASET}
    DBClient,
    {$IFDEF DRIVERRESTFUL}
      ormbr.restdataset.clientdataset
    {$ELSE}
      ormbr.dataset.clientdataset
    {$ENDIF},
  {$ENDIF}

  // ORMBr Interface
  {$IFDEF DRIVERRESTFUL}
    ormbr.restfactory.interfaces
  {$ELSE}
    dbebr.factory.interfaces
  {$ENDIF},
  ormbr.dataset.base.adapter;

type
  {$IFDEF DRIVERRESTFUL}
    IMDConnection = IRESTConnection
  {$ELSE}
    IMDConnection = IDBConnection
  {$ENDIF};

  TManagerDataSet = class
  private
    FConnection: IMDConnection;
    FRepository: TDictionary<string, TObject>;
    FNestedList: TDictionary<string, TObjectList<TObject>>;
    FOwnerNestedList: Boolean;
    function Resolver<T: class, constructor>: TDataSetBaseAdapter<T>;
    procedure ResolverDataSetType(const ADataSet: TDataSet);
  public
    constructor Create(const AConnection: IMDConnection);
    destructor Destroy; override;
    {$IFNDEF DRIVERRESTFUL}
    procedure NextPacket<T: class, constructor>;
    function GetAutoNextPacket<T: class, constructor>: Boolean;
    procedure SetAutoNextPacket<T: class, constructor>(const AValue: Boolean);
    {$ENDIF}
    procedure RemoveAdapter<T: class>;
    function AddAdapter<T: class, constructor>(const ADataSet: TDataSet;
      const APageSize: Integer = -1): TManagerDataSet; overload;
    function AddAdapter<T, M: class, constructor>(const ADataSet: TDataSet): TManagerDataSet; overload;
    function AddLookupField<T, M: class, constructor>(const AFieldName: string;
                                                      const AKeyFields: string;
                                                      const ALookupKeyFields: string;
                                                      const ALookupResultField: string;
                                                      const ADisplayLabel: string = ''): TManagerDataSet;
    procedure Open<T: class, constructor>; overload;
    procedure Open<T: class, constructor>(const AID: Integer); overload;
    procedure Open<T: class, constructor>(const AID: String); overload;
    procedure OpenWhere<T: class, constructor>(const AWhere: string; const AOrderBy: string = '');
    procedure Close<T: class, constructor>;
    procedure LoadLazy<T: class, constructor>(const AOwner: T);
    procedure RefreshRecord<T: class, constructor>;
    procedure EmptyDataSet<T: class, constructor>;
    procedure CancelUpdates<T: class, constructor>;
    procedure ApplyUpdates<T: class, constructor>(const MaxErros: Integer);
    procedure Save<T: class, constructor>(AObject: T);
    function Current<T: class, constructor>: T;
    function DataSet<T: class, constructor>: TDataSet;
    // ObjectSet
    function Find<T: class, constructor>: TObjectList<T>; overload;
    function Find<T: class, constructor>(const AID: TValue): T; overload;
    function FindWhere<T: class, constructor>(const AWhere: string;
                                              const AOrderBy: string = ''): TObjectList<T>;
    function NestedList<T: class>: TObjectList<T>;
    function AutoNextPacket<T: class, constructor>(const AValue: Boolean): TManagerDataSet;
    property OwnerNestedList: Boolean read FOwnerNestedList write FOwnerNestedList;
  end;

implementation

{ TManagerDataSet }

constructor TManagerDataSet.Create(const AConnection: IMDConnection);
begin
  FConnection := AConnection;
  FRepository := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
  FNestedList := TObjectDictionary<string, TObjectList<TObject>>.Create([doOwnsValues]);
  FOwnerNestedList := False;
end;

destructor TManagerDataSet.Destroy;
begin
  FNestedList.Free;
  FRepository.Free;
  inherited;
end;

function TManagerDataSet.Current<T>: T;
begin
  Result := Resolver<T>.Current;
end;

function TManagerDataSet.NestedList<T>: TObjectList<T>;
var
  LClassName: String;
begin
  Result := nil;
  LClassName := TClass(T).ClassName;
  if FNestedList.ContainsKey(LClassName) then
    Result := TObjectList<T>(FNestedList.Items[LClassName]);
end;

function TManagerDataSet.DataSet<T>: TDataSet;
begin
  Result := Resolver<T>.FOrmDataSet;
end;

procedure TManagerDataSet.EmptyDataSet<T>;
begin
  Resolver<T>.EmptyDataSet;
end;

function TManagerDataSet.Find<T>(const AID: TValue): T;
begin
  if AID.IsType<integer> then
    Result := Resolver<T>.Find(AID.AsType<integer>)
  else
  if AID.IsType<string> then
    Result := Resolver<T>.Find(AID.AsType<string>)
  else
    raise Exception.Create('Invalid parameter type');
end;

function TManagerDataSet.Find<T>: TObjectList<T>;
var
  LObjectList: TObjectList<T>;
begin
  Result := nil;
  if not FOwnerNestedList then
  begin
    Result := Resolver<T>.Find;
    Exit;
  end;
  LObjectList := Resolver<T>.Find;
  // Limpa a lista de objectos
  FNestedList.AddOrSetValue(TClass(T).ClassName, TObjectList<TObject>(LObjectList));
end;

procedure TManagerDataSet.CancelUpdates<T>;
begin
  Resolver<T>.CancelUpdates;
end;

procedure TManagerDataSet.Close<T>;
begin
  Resolver<T>.EmptyDataSet;
end;

procedure TManagerDataSet.LoadLazy<T>(const AOwner: T);
begin
  Resolver<T>.LoadLazy(AOwner);
end;

function TManagerDataSet.AddAdapter<T, M>(const ADataSet: TDataSet): TManagerDataSet;
var
  LDataSetAdapter: TDataSetBaseAdapter<T>;
  LMaster: TDataSetBaseAdapter<T>;
  LClassName: String;
  LMasterName: String;
begin
  Result := Self;
  LClassName := TClass(T).ClassName;
  LMasterName := TClass(M).ClassName;
  if FRepository.ContainsKey(LClassName) then
    Exit;
  if not FRepository.ContainsKey(LMasterName) then
    Exit;
  LMaster := TDataSetBaseAdapter<T>(FRepository.Items[LMasterName]);
  if LMaster = nil then
    Exit;

  // Checagem do tipo do dataset definido para uso
  ResolverDataSetType(ADataSet);
  {$IFDEF DRIVERRESTFUL}
    {$IFDEF USEFDMEMTABLE}
      LDataSetAdapter := TRESTFDMemTableAdapter<T>
                           .Create(FConnection, ADataSet, -1, LMaster);
    {$ELSE}
      LDataSetAdapter := TRESTClientDataSetAdapter<T>
                           .Create(FConnection, ADataSet, -1, LMaster);
    {$ENDIF}
  {$ELSE}
    {$IFDEF USEFDMEMTABLE}
      LDataSetAdapter := TFDMemTableAdapter<T>
                           .Create(FConnection, ADataSet, -1, LMaster);
    {$ELSE}
      LDataSetAdapter := TClientDataSetAdapter<T>
                           .Create(FConnection, ADataSet, -1, LMaster);
    {$ENDIF}
  {$ENDIF}
  // Adiciona o container ao repositório
  FRepository.Add(LClassName, LDataSetAdapter);
end;

function TManagerDataSet.AddAdapter<T>(const ADataSet: TDataSet;
  const APageSize: Integer): TManagerDataSet;
var
  LDataSetAdapter: TDataSetBaseAdapter<T>;
  LClassName: String;
begin
  Result := Self;
  LClassName := TClass(T).ClassName;
  if FRepository.ContainsKey(LClassName) then
    Exit;

  // Checagem do tipo do dataset definido para uso
  ResolverDataSetType(ADataSet);
  {$IFDEF DRIVERRESTFUL}
    {$IFDEF USEFDMEMTABLE}
      LDataSetAdapter := TRESTFDMemTableAdapter<T>
                           .Create(FConnection, ADataSet, APageSize, nil);
    {$ELSE}
      LDataSetAdapter := TRESTClientDataSetAdapter<T>
                           .Create(FConnection, ADataSet, APageSize, nil);
    {$ENDIF}
  {$ELSE}
    {$IFDEF USEFDMEMTABLE}
      LDataSetAdapter := TFDMemTableAdapter<T>
                           .Create(FConnection, ADataSet, APageSize, nil);
    {$ELSE}
      LDataSetAdapter := TClientDataSetAdapter<T>
                           .Create(FConnection, ADataSet, APageSize, nil);
    {$ENDIF}
  {$ENDIF}
  // Adiciona o container ao repositório
  FRepository.Add(LClassName, LDataSetAdapter);
end;

function TManagerDataSet.AddLookupField<T, M>(
  const AFieldName, AKeyFields: string;
  const ALookupKeyFields, ALookupResultField, ADisplayLabel: string): TManagerDataSet;
var
  LObject: TDataSetBaseAdapter<M>;
begin
  Result := Self;
  LObject := Resolver<M>;
  if LObject = nil then
    Exit;
  Resolver<T>.AddLookupField(AFieldName,
                             AKeyFields,
                             LObject,
                             ALookupKeyFields,
                             ALookupResultField,
                             ADisplayLabel);
end;

procedure TManagerDataSet.ApplyUpdates<T>(const MaxErros: Integer);
begin
  Resolver<T>.ApplyUpdates(MaxErros);
end;

function TManagerDataSet.AutoNextPacket<T>(const AValue: Boolean): TManagerDataSet;
begin
  Resolver<T>.AutoNextPacket := AValue;
end;

procedure TManagerDataSet.Open<T>(const AID: String);
begin
  Resolver<T>.OpenIDInternal(AID);
end;

procedure TManagerDataSet.OpenWhere<T>(const AWhere,
  AOrderBy: string);
begin
  Resolver<T>.OpenWhereInternal(AWhere, AOrderBy);
end;

procedure TManagerDataSet.Open<T>(const AID: Integer);
begin
  Resolver<T>.OpenIDInternal(AID);
end;

procedure TManagerDataSet.Open<T>;
begin
  Resolver<T>.OpenSQLInternal('');
end;

procedure TManagerDataSet.RefreshRecord<T>;
begin
  Resolver<T>.RefreshRecord;
end;

procedure TManagerDataSet.RemoveAdapter<T>;
var
  LClassName: String;
begin
  LClassName := TClass(T).ClassName;
  if not FRepository.ContainsKey(LClassName) then
    Exit;

  FRepository.Remove(LClassName);
  FRepository.TrimExcess;
end;

function TManagerDataSet.Resolver<T>: TDataSetBaseAdapter<T>;
var
  LClassName: String;
begin
  Result := nil;
  LClassName := TClass(T).ClassName;
  if FRepository.ContainsKey(LClassName) then
    Result := TDataSetBaseAdapter<T>(FRepository.Items[LClassName]);
end;

procedure TManagerDataSet.ResolverDataSetType(const ADataSet: TDataSet);
begin
  {$IFDEF USEFDMEMTABLE}
    if not (ADataSet is TFDMemTable) then
      raise Exception.Create('Is not TFDMemTable type');
  {$ENDIF}
  {$IFDEF USECLIENTDATASET}
    if not (ADataSet is TClientDataSet) then
      raise Exception.Create('Is not TClientDataSet type');
  {$ENDIF}
  {$IFNDEF USEMEMDATASET}
    raise Exception.Create('Enable the directive "USEFDMEMTABLE" or "USECLIENTDATASET" in file ormbr.inc');
  {$ENDIF}
end;

procedure TManagerDataSet.Save<T>(AObject: T);
begin
  Resolver<T>.Save(AObject);
end;

function TManagerDataSet.FindWhere<T>(const AWhere, AOrderBy: string): TObjectList<T>;
var
  LObjectList: TObjectList<T>;
begin
  Result := nil;
  if not FOwnerNestedList then
  begin
    Result := Resolver<T>.FindWhere(AWhere, AOrderBy);
    Exit;
  end;
  LObjectList := Resolver<T>.FindWhere(AWhere, AOrderBy);
  // Limpa a lista de objectos
  FNestedList.AddOrSetValue(TClass(T).ClassName, TObjectList<TObject>(LObjectList));
end;

{$IFNDEF DRIVERRESTFUL}
procedure TManagerDataSet.NextPacket<T>;
begin
  Resolver<T>.NextPacket;
end;

function TManagerDataSet.GetAutoNextPacket<T>: Boolean;
begin
  Result := Resolver<T>.AutoNextPacket;
end;

procedure TManagerDataSet.SetAutoNextPacket<T>(const AValue: Boolean);
begin
  Resolver<T>.AutoNextPacket := AValue;
end;
{$ENDIF}

end.

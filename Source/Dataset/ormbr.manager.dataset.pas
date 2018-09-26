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

unit ormbr.manager.dataset;

interface

uses
  DB,
  SysUtils,
  Generics.Collections,
  {$IFDEF USEFDMEMTABLE}
  FireDAC.Comp.Client,
    {$IFDEF DRIVERRESTFUL}
    ormbr.restdataset.fdmemtable,
    {$ELSE}
    ormbr.dataset.fdmemtable,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF USECLIENTDATASET}
  DBClient,
    {$IFDEF DRIVERRESTFUL}
    ormbr.restdataset.clientdataset,
    {$ELSE}
    ormbr.dataset.clientdataset,
    {$ENDIF}
  {$ENDIF}
  /// ORMBr Interface
  {$IFDEF DRIVERRESTFUL}
  ormbr.client.interfaces,
  {$ELSE}
  ormbr.factory.interfaces,
  {$ENDIF}
  ormbr.dataset.base.adapter;

type
  TManagerDataSet = class
  private
    FConnection: {$IFDEF DRIVERRESTFUL}IRESTConnection;
                 {$ELSE}IDBConnection;
                 {$ENDIF}
    FRepository: TDictionary<string, TObject>;
    FNestedList: TDictionary<string, TObjectList<TObject>>;
    function Resolver<T: class, constructor>: TDataSetBaseAdapter<T>;
  public
    constructor Create(const AConnection: {$IFDEF DRIVERRESTFUL}IRESTConnection);
                                          {$ELSE}IDBConnection);
                                          {$ENDIF}
    {$IFNDEF DRIVERRESTFUL}
    function NextPacket<T: class, constructor>: TManagerDataSet;
    function GetAutoNextPacket<T: class, constructor>: Boolean;
    procedure SetAutoNextPacket<T: class, constructor>(const AValue: Boolean);
    {$ENDIF}
    destructor Destroy; override;
    function AddAdapter<T: class, constructor>(ADataSet: TDataSet;
      const APageSize: Integer = -1): TManagerDataSet; overload;
    function AddAdapter<T, M: class, constructor>(ADataSet: TDataSet): TManagerDataSet; overload;
    function Open<T: class, constructor>: TManagerDataSet; overload;
    function Open<T: class, constructor>(const AID: Integer): TManagerDataSet; overload;
    function Open<T: class, constructor>(const AID: String): TManagerDataSet; overload;
    function OpenWhere<T: class, constructor>(const AWhere: string; const AOrderBy: string = ''): TManagerDataSet;
    function Close<T: class, constructor>: TManagerDataSet;
    function Save<T: class, constructor>(AObject: T): TManagerDataSet;
    function LoadLazy<T: class, constructor>(const AOwner: T): TManagerDataSet;
    function RefreshRecord<T: class, constructor>: TManagerDataSet;
    function EmptyDataSet<T: class, constructor>: TManagerDataSet;
    function CancelUpdates<T: class, constructor>: TManagerDataSet;
    function ApplyUpdates<T: class, constructor>(const MaxErros: Integer): TManagerDataSet;
    function AddLookupField<T, M: class, constructor>(const AFieldName: string;
                                                      const AKeyFields: string;
                                                      const ALookupKeyFields: string;
                                                      const ALookupResultField: string;
                                                      const ADisplayLabel: string = ''): TManagerDataSet;
    function Current<T: class, constructor>: T;
    function DataSet<T: class, constructor>: TDataSet;
    /// ObjectSet
    function Find<T: class, constructor>: TManagerDataSet; overload;
    function Find<T: class, constructor>(const AID: Integer): T; overload;
    function Find<T: class, constructor>(const AID: String): T; overload;
    function FindWhere<T: class, constructor>(const AWhere: string;
                                              const AOrderBy: string = ''): TManagerDataSet;
    function NestedList<T: class>: TObjectList<T>;
  end;

implementation

{ TManagerDataSet }

constructor TManagerDataSet.Create(const AConnection: {$IFDEF DRIVERRESTFUL}IRESTConnection);
                                                      {$ELSE}IDBConnection);
                                                      {$ENDIF}
begin
  FConnection := AConnection;
  FRepository := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
  FNestedList := TObjectDictionary<string, TObjectList<TObject>>.Create([doOwnsValues]);
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
  LClassName := TClass(T).ClassName;
  if FNestedList.ContainsKey(LClassName) then
    Result := TObjectList<T>(FNestedList.Items[LClassName]);
end;

function TManagerDataSet.DataSet<T>: TDataSet;
begin
  Result := Resolver<T>.FOrmDataSet;
end;

function TManagerDataSet.EmptyDataSet<T>: TManagerDataSet;
begin
  Resolver<T>.EmptyDataSet;
  Result := Self;
end;

function TManagerDataSet.Find<T>(const AID: Integer): T;
begin
  Result := Resolver<T>.Find(AID);
end;

function TManagerDataSet.Find<T>(const AID: String): T;
begin
  Result := Resolver<T>.Find(AID);
end;

function TManagerDataSet.Find<T>: TManagerDataSet;
var
  LObjectList: TObjectList<T>;
begin
  LObjectList := Resolver<T>.Find;
  /// <summary> Limpa a lista de objectos </summary>
  FNestedList.AddOrSetValue(TClass(T).ClassName, TObjectList<TObject>(LObjectList));
  Result := Self;
end;

function TManagerDataSet.CancelUpdates<T>: TManagerDataSet;
begin
  Resolver<T>.CancelUpdates;
  Result := Self;
end;

function TManagerDataSet.Close<T>: TManagerDataSet;
begin
  Resolver<T>.EmptyDataSet;
  Result := Self;
end;

function TManagerDataSet.LoadLazy<T>(const AOwner: T): TManagerDataSet;
begin
  Resolver<T>.LoadLazy(AOwner);
  Result := Self;
end;

function TManagerDataSet.AddAdapter<T, M>(ADataSet: TDataSet): TManagerDataSet;
var
  LDataSetAdapter: TDataSetBaseAdapter<T>;
  LMaster: TDataSetBaseAdapter<T>;
  LClassName: String;
  LMasterName: String;
begin
  LClassName := TClass(T).ClassName;
  LMasterName := TClass(M).ClassName;
  if not FRepository.ContainsKey(LClassName) then
  begin
    if FRepository.ContainsKey(LMasterName) then
    begin
      LMaster := TDataSetBaseAdapter<T>(FRepository.Items[LMasterName]);
      if LMaster <> nil then
      begin
        /// <summary> Checagem do tipo do dataset definido para uso </summary>
        {$IFDEF USEFDMEMTABLE}
          if ADataSet is TFDMemTable then
            {$IFDEF DRIVERRESTFUL}
            LDataSetAdapter := TRESTFDMemTableAdapter<T>.Create(FConnection, ADataSet, -1, LMaster)
            {$ELSE}
            LDataSetAdapter := TFDMemTableAdapter<T>.Create(FConnection, ADataSet, -1, LMaster)
            {$ENDIF}
          else
            raise Exception.Create('Is not TFDMemTable type');
        {$ENDIF}
        {$IFDEF USECLIENTDATASET}
          if ADataSet is TClientDataSet then
            {$IFDEF DRIVERRESTFUL}
            LDataSetAdapter := TRESTClientDataSetAdapter<T>.Create(FConnection, ADataSet, -1, LMaster)
            {$ELSE}
            LDataSetAdapter := TClientDataSetAdapter<T>.Create(FConnection, ADataSet, -1, LMaster)
            {$ENDIF}
          else
            raise Exception.Create('Is not TClientDataSet type');
        {$ENDIF}
        {$IFNDEF USEMEMDATASET}
          raise Exception.Create('Enable the directive "USEFDMEMTABLE" or "USECLIENTDATASET" in file ormbr.inc');
        {$ENDIF}
        /// <summary> Adiciona o container ao repositório </summary>
        FRepository.Add(LClassName, LDataSetAdapter);
      end;
    end;
  end;
  Result := Self;
end;

function TManagerDataSet.AddAdapter<T>(ADataSet: TDataSet;
  const APageSize: Integer): TManagerDataSet;
var
  LDataSetAdapter: TDataSetBaseAdapter<T>;
  LClassName: String;
begin
  LClassName := TClass(T).ClassName;
  if not FRepository.ContainsKey(LClassName) then
  begin
    {$IFDEF USEFDMEMTABLE}
      if ADataSet is TFDMemTable then
        {$IFDEF DRIVERRESTFUL}
        LDataSetAdapter := TRESTFDMemTableAdapter<T>.Create(FConnection, ADataSet, APageSize, nil)
        {$ELSE}
        LDataSetAdapter := TFDMemTableAdapter<T>.Create(FConnection, ADataSet, APageSize, nil)
        {$ENDIF}
      else
        raise Exception.Create('Is not TFDMemTable type');
    {$ENDIF}
    {$IFDEF USECLIENTDATASET}
      if ADataSet is TClientDataSet then
        {$IFDEF DRIVERRESTFUL}
        LDataSetAdapter := TRESTClientDataSetAdapter<T>.Create(FConnection, ADataSet, APageSize, nil)
        {$ELSE}
        LDataSetAdapter := TClientDataSetAdapter<T>.Create(FConnection, ADataSet, APageSize, nil)
        {$ENDIF}
      else
        raise Exception.Create('Is not TClientDataSet type');
    {$ENDIF}
    {$IFNDEF USEMEMDATASET}
      raise Exception.Create('Enable the directive "USEFDMEMTABLE" or "USECLIENTDATASET" in file ormbr.inc');
    {$ENDIF}
    /// <summary> Adiciona o container ao repositório </summary>
    FRepository.Add(LClassName, LDataSetAdapter);
  end;
  Result := Self;
end;

function TManagerDataSet.AddLookupField<T, M>(const AFieldName, AKeyFields: string;
  const ALookupKeyFields, ALookupResultField, ADisplayLabel: string): TManagerDataSet;
var
  LObject: TDataSetBaseAdapter<M>;
begin
  LObject := Resolver<M>;
  if LObject <> nil then
  begin
    Resolver<T>.AddLookupField(AFieldName,
                               AKeyFields,
                               LObject,
                               ALookupKeyFields,
                               ALookupResultField,
                               ADisplayLabel);
  end;
  Result := Self;
end;

function TManagerDataSet.ApplyUpdates<T>(const MaxErros: Integer): TManagerDataSet;
begin
  Resolver<T>.ApplyUpdates(MaxErros);
  Result := Self;
end;

function TManagerDataSet.Open<T>(const AID: String): TManagerDataSet;
begin
  Resolver<T>.OpenIDInternal(AID);
  Result := Self;
end;

function TManagerDataSet.OpenWhere<T>(const AWhere,
  AOrderBy: string): TManagerDataSet;
begin
  Resolver<T>.OpenWhereInternal(AWhere, AOrderBy);
  Result := Self;
end;

function TManagerDataSet.Open<T>(const AID: Integer): TManagerDataSet;
begin
  Resolver<T>.OpenIDInternal(AID);
  Result := Self;
end;

function TManagerDataSet.Open<T>: TManagerDataSet;
begin
  Resolver<T>.OpenSQLInternal('');
  Result := Self;
end;

function TManagerDataSet.RefreshRecord<T>: TManagerDataSet;
begin
  Resolver<T>.RefreshRecord;
  Result := Self;
end;

function TManagerDataSet.Resolver<T>: TDataSetBaseAdapter<T>;
var
  LClassName: String;
begin
  LClassName := TClass(T).ClassName;
  if FRepository.ContainsKey(LClassName) then
    Result := TDataSetBaseAdapter<T>(FRepository.Items[LClassName]);
end;

function TManagerDataSet.Save<T>(AObject: T): TManagerDataSet;
begin
  Resolver<T>.Save(AObject);
  Result := Self;
end;

function TManagerDataSet.FindWhere<T>(const AWhere, AOrderBy: string): TManagerDataSet;
var
  LObjectList: TObjectList<T>;
begin
  LObjectList := Resolver<T>.FindWhere(AWhere, AOrderBy);
  /// <summary> Limpa a lista de objectos </summary>
  FNestedList.AddOrSetValue(TClass(T).ClassName, TObjectList<TObject>(LObjectList));
  Result := Self;
end;

{$IFNDEF DRIVERRESTFUL}
function TManagerDataSet.NextPacket<T>: TManagerDataSet;
begin
  Resolver<T>.NextPacket;
  Result := Self;
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

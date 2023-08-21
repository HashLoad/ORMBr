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

unit ormbr.db.manager.fdmemtable;

interface

uses
  DB,
  Rtti,
  Classes,
  SysUtils,
  Variants,
  Generics.Collections,
  FireDAC.Comp.Client,
  dbebr.factory.interfaces,
  ormbr.dataset.base.adapter,
  ormbr.dataset.fdmemtable;

type
  TManagerFDMemTable = class
  private
    FNestedList: TDictionary<string, TObjectList<TObject>>;
    FOwnerNestedList: Boolean;
    FConnection: IDBConnection;
    FRepository: TDictionary<string, TObject>;
    function Resolver<T: class, constructor>: TDataSetBaseAdapter<T>;
  public
    constructor Create(const AConnection: IDBConnection);
    destructor Destroy; override;
    function AddAdapter<T: class, constructor>(const ADataSet: TDataSet;
      const APageSize: Integer = -1): TManagerFDMemTable; overload;
    function AddAdapter<T, M: class, constructor>(const ADataSet: TDataSet): TManagerFDMemTable; overload;
    function AddLookupField<T, M: class, constructor>(const AFieldName: string;
                                                      const AKeyFields: string;
                                                      const ALookupKeyFields: string;
                                                      const ALookupResultField: string;
                                                      const ADisplayLabel: string = ''): TManagerFDMemTable;
    procedure RemoveAdapter<T: class>;
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
    /// ObjectSet
    function Find<T: class, constructor>: TObjectList<T>; overload;
    function Find<T: class, constructor>(const AID: TValue): T; overload;
    function FindWhere<T: class, constructor>(const AWhere: string;
                                              const AOrderBy: string = ''): TObjectList<T>;
    function NestedList<T: class>: TObjectList<T>;
    function AutoNextPacket<T: class, constructor>(const AValue: Boolean): TManagerFDMemTable;
    property OwnerNestedList: Boolean read FOwnerNestedList write FOwnerNestedList;
  end;

implementation

{ TManagerFDMemTable }

constructor TManagerFDMemTable.Create(const AConnection: IDBConnection);
begin
  FConnection := AConnection;
  FRepository := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
  FNestedList := TObjectDictionary<string, TObjectList<TObject>>.Create([doOwnsValues]);
  FOwnerNestedList := False;
end;

destructor TManagerFDMemTable.Destroy;
begin
  FNestedList.Free;
  FRepository.Free;
  inherited;
end;

function TManagerFDMemTable.Current<T>: T;
begin
  Result := Resolver<T>.Current;
end;

function TManagerFDMemTable.NestedList<T>: TObjectList<T>;
var
  LClassName: String;
begin
  Result := nil;
  LClassName := TClass(T).ClassName;
  if FNestedList.ContainsKey(LClassName) then
    Result := TObjectList<T>(FNestedList.Items[LClassName]);
end;

function TManagerFDMemTable.DataSet<T>: TDataSet;
begin
  Result := Resolver<T>.FOrmDataSet;
end;

procedure TManagerFDMemTable.EmptyDataSet<T>;
begin
  Resolver<T>.EmptyDataSet;
end;

function TManagerFDMemTable.Find<T>(const AID: TValue): T;
begin
  if AID.IsType<integer> then
    Result := Resolver<T>.Find(AID.AsType<integer>)
  else
  if AID.IsType<string> then
    Result := Resolver<T>.Find(AID.AsType<integer>)
end;

function TManagerFDMemTable.Find<T>: TObjectList<T>;
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
  /// <summary> Limpa a lista de objectos </summary>
  FNestedList.AddOrSetValue(TClass(T).ClassName, TObjectList<TObject>(LObjectList));
end;

procedure TManagerFDMemTable.CancelUpdates<T>;
begin
  Resolver<T>.CancelUpdates;
end;

procedure TManagerFDMemTable.Close<T>;
begin
  Resolver<T>.EmptyDataSet;
end;

procedure TManagerFDMemTable.LoadLazy<T>(const AOwner: T);
begin
  Resolver<T>.LoadLazy(AOwner);
end;

function TManagerFDMemTable.AddLookupField<T, M>(const AFieldName, AKeyFields: string;
  const ALookupKeyFields, ALookupResultField, ADisplayLabel: string): TManagerFDMemTable;
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

procedure TManagerFDMemTable.ApplyUpdates<T>(const MaxErros: Integer);
begin
  Resolver<T>.ApplyUpdates(MaxErros);
end;

function TManagerFDMemTable.AutoNextPacket<T>(const AValue: Boolean): TManagerFDMemTable;
begin
  Resolver<T>.AutoNextPacket := AValue;
end;

procedure TManagerFDMemTable.Open<T>(const AID: String);
begin
  Resolver<T>.OpenIDInternal(AID);
end;

procedure TManagerFDMemTable.OpenWhere<T>(const AWhere,
  AOrderBy: string);
begin
  Resolver<T>.OpenWhereInternal(AWhere, AOrderBy);
end;

procedure TManagerFDMemTable.Open<T>(const AID: Integer);
begin
  Resolver<T>.OpenIDInternal(AID);
end;

procedure TManagerFDMemTable.Open<T>;
begin
  Resolver<T>.OpenSQLInternal('');
end;

procedure TManagerFDMemTable.RefreshRecord<T>;
begin
  Resolver<T>.RefreshRecord;
end;

procedure TManagerFDMemTable.RemoveAdapter<T>;
var
  LClassName: String;
begin
  LClassName := TClass(T).ClassName;
  if not FRepository.ContainsKey(LClassName) then
    Exit;

  FRepository.Remove(LClassName);
  FRepository.TrimExcess;
end;

function TManagerFDMemTable.Resolver<T>: TDataSetBaseAdapter<T>;
var
  LClassName: String;
begin
  Result := nil;
  LClassName := TClass(T).ClassName;
  if FRepository.ContainsKey(LClassName) then
    Result := TDataSetBaseAdapter<T>(FRepository.Items[LClassName]);
end;

procedure TManagerFDMemTable.Save<T>(AObject: T);
begin
  Resolver<T>.Save(AObject);
end;

function TManagerFDMemTable.FindWhere<T>(const AWhere, AOrderBy: string): TObjectList<T>;
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
  /// <summary> Limpa a lista de objectos </summary>
  FNestedList.AddOrSetValue(TClass(T).ClassName, TObjectList<TObject>(LObjectList));
end;

function TManagerFDMemTable.AddAdapter<T, M>(const ADataSet: TDataSet): TManagerFDMemTable;
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
  /// <summary> Checagem do tipo do dataset definido para uso </summary>
  if ADataSet is TFDMemTable then
    LDataSetAdapter := TFDMemTableAdapter<T>.Create(FConnection, ADataSet, -1, LMaster)
  else
    raise Exception.Create('Is not TFDMemTable type');
  /// <summary> Adiciona o container ao repositório </summary>
  FRepository.Add(LClassName, LDataSetAdapter);
end;

function TManagerFDMemTable.AddAdapter<T>(const ADataSet: TDataSet;
  const APageSize: Integer): TManagerFDMemTable;
var
  LDataSetAdapter: TDataSetBaseAdapter<T>;
  LClassName: String;
begin
  Result := Self;
  LClassName := TClass(T).ClassName;
  if FRepository.ContainsKey(LClassName) then
    Exit;
  if ADataSet is TFDMemTable then
    LDataSetAdapter := TFDMemTableAdapter<T>.Create(FConnection, ADataSet, APageSize, nil)
  else
    raise Exception.Create('Is not TFDMemTable type');
  /// <summary> Adiciona o container ao repositório </summary>
  FRepository.Add(LClassName, LDataSetAdapter);
end;

end.

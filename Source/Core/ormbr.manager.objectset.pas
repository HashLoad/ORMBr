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

unit ormbr.manager.objectset;

interface

uses
  DB,
  DBClient,
  Generics.Collections,
  ormbr.factory.interfaces,
  ormbr.objectset.adapter;

type
  TManagerDataSet = class
  private
    FConnection: IDBConnection;
    FRepository: TDictionary<string, TObject>;
    FDataList: TDictionary<string, TObjectList<TObject>>;
    function Resolver<T: class, constructor>: TObjectSetAdapter<T>;
    procedure DataListFree;
    procedure RepositoryListFree;
  public
    constructor Create(const AConnection: IDBConnection);
    destructor Destroy; override;
    function AddAdapter<T: class, constructor>(const APageSize: Integer = -1): TManagerDataSet;
    function DataList<T: class>: TObjectList<T>;
    /// ObjectSet
    function Find<T: class, constructor>: TManagerDataSet; overload;
    function Find<T: class, constructor>(const AID: Integer): T; overload;
    function Find<T: class, constructor>(const AID: String): T; overload;
    function FindWhere<T: class, constructor>(const AWhere: string;
                                              const AOrderBy: string = ''): TManagerDataSet;
    function ModifiedFields<T: class, constructor>: TDictionary<string, TList<string>>;
    function ExistSequence<T: class, constructor>: Boolean;
    function Insert<T: class, constructor>(const AObject: T): TManagerDataSet;
    function Update<T: class, constructor>(const AObject: T): TManagerDataSet;
    function Delete<T: class, constructor>(const AObject: T): TManagerDataSet;
    function Modify<T: class, constructor>(const AObject: T): TManagerDataSet;
    function LoadLazy<T: class, constructor>(const AOwner, AObject: TObject): TManagerDataSet;
    function NextPacket<T: class, constructor>: TManagerDataSet;
  end;

implementation

{ TManagerDataSet }

constructor TManagerDataSet.Create(const AConnection: IDBConnection);
begin
  FConnection := AConnection;
  FRepository := TDictionary<string, TObject>.Create;
  FDataList := TDictionary<string, TObjectList<TObject>>.Create;
end;

function TManagerDataSet.DataList<T>: TObjectList<T>;
var
  LClassName: String;
begin
  LClassName := TClass(T).ClassName;
  if FDataList.ContainsKey(LClassName) then
  begin
    Result := TObjectList<T>(FDataList.Items[LClassName]);
  end;
end;

procedure TManagerDataSet.DataListFree;
var
  LObjectList: TObjectList<TObject>;
begin
  for LObjectList in FDataList.Values do
  begin
    LObjectList.Clear;
    LObjectList.Free;
  end;
end;

function TManagerDataSet.Delete<T>(const AObject: T): TManagerDataSet;
begin
  Resolver<T>.Delete(AObject);
  Result := Self;
end;

destructor TManagerDataSet.Destroy;
begin
  DataListFree;
  RepositoryListFree;
  FDataList.Free;
  FRepository.Free;
  inherited;
end;

function TManagerDataSet.ExistSequence<T>: Boolean;
begin
  Result := Resolver<T>.ExistSequence;
end;

function TManagerDataSet.Find<T>(const AID: Integer): T;
begin
  Result := Resolver<T>.Find(AID);
end;

function TManagerDataSet.Find<T>(const AID: String): T;
begin
  Result := Resolver<T>.Find(AID);
end;

function TManagerDataSet.AddAdapter<T>(const APageSize: Integer): TManagerDataSet;
var
  LContainer: TObjectSetAdapter<T>;
  LClassName: String;
begin
  LClassName := TClass(T).ClassName;
  if not FRepository.ContainsKey(LClassName) then
  begin
    LContainer := TObjectSetAdapter<T>.Create(FConnection, APageSize);
    /// <summary> Adiciona o container ao repositório </summary>
    FRepository.Add(LClassName, LContainer);
  end;
  Result := Self;
end;

function TManagerDataSet.Find<T>: TManagerDataSet;
var
  LObjectList: TObjectList<T>;
begin
  LObjectList := Resolver<T>.Find;
  /// <summary> Limpa a lista de objectos </summary>
  DataListFree;
  FDataList.AddOrSetValue(TClass(T).ClassName, TObjectList<TObject>(LObjectList));
  Result := Self;
end;

procedure TManagerDataSet.RepositoryListFree;
var
  LObject: TObject;
begin
  for LObject in FRepository.Values do
    LObject.Free;
end;

function TManagerDataSet.Resolver<T>: TObjectSetAdapter<T>;
var
  LClassName: String;
begin
  LClassName := TClass(T).ClassName;
  if FRepository.ContainsKey(LClassName) then
    Result := TObjectSetAdapter<T>(FRepository.Items[LClassName]);
end;

function TManagerDataSet.Update<T>(const AObject: T): TManagerDataSet;
begin
  Resolver<T>.Update(AObject);
  Result := Self;
end;

function TManagerDataSet.FindWhere<T>(const AWhere, AOrderBy: string): TManagerDataSet;
var
  LObjectList: TObjectList<T>;
begin
  LObjectList := Resolver<T>.FindWhere(AWhere, AOrderBy);
  /// <summary> Limpa a lista de objectos </summary>
  DataListFree;
  FDataList.AddOrSetValue(TClass(T).ClassName, TObjectList<TObject>(LObjectList));
  Result := Self;
end;

function TManagerDataSet.Insert<T>(const AObject: T): TManagerDataSet;
begin
  Resolver<T>.Insert(AObject);
  Result := Self;
end;

function TManagerDataSet.LoadLazy<T>(const AOwner, AObject: TObject): TManagerDataSet;
begin
  Resolver<T>.LoadLazy(AOwner, AObject);
  Result := Self;
end;

function TManagerDataSet.ModifiedFields<T>: TDictionary<string, TList<string>>;
begin
  Result := Resolver<T>.ModifiedFields;
end;

function TManagerDataSet.Modify<T>(const AObject: T): TManagerDataSet;
begin
  Resolver<T>.Modify(AObject);
  Result := Self;
end;

function TManagerDataSet.NextPacket<T>: TManagerDataSet;
var
  LObjectList: TObjectList<T>;
begin
  LObjectList := TObjectList<T>(FDataList.Items[TClass(T).ClassName]);
  Resolver<T>.NextPacket(LObjectList);
  Result := Self;
end;

end.

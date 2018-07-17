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
  Generics.Collections,
  /// ORMBr
  {$IFDEF DRIVERRESTFUL}
  ormbr.restobjectset.adapter,
  ormbr.client.interfaces,
  {$ELSE}
  ormbr.objectset.adapter,
  ormbr.factory.interfaces,
  {$ENDIF}
  ormbr.objectset.base.adapter;

type
  TManagerObjectSet = class
  private
    FConnection: {$IFDEF DRIVERRESTFUL}IRESTConnection;
                 {$ELSE}IDBConnection;
                 {$ENDIF}
    FRepository: TDictionary<string, TObject>;
    FNestedList: TDictionary<string, TObjectList<TObject>>;
    function Resolver<T: class, constructor>: TObjectSetBaseAdapter<T>;
  public

    constructor Create(const AConnection: {$IFDEF DRIVERRESTFUL}IRESTConnection);
                                          {$ELSE}IDBConnection);
                                          {$ENDIF}
    destructor Destroy; override;
    function AddAdapter<T: class, constructor>(const APageSize: Integer = -1): TManagerObjectSet;
    /// ObjectSet
    function Find<T: class, constructor>: TManagerObjectSet; overload;
    function Find<T: class, constructor>(const AID: Integer): T; overload;
    function Find<T: class, constructor>(const AID: String): T; overload;
    {$IFDEF DRIVERRESTFUL}
    function Find<T: class, constructor>(const AMethodName: String; const AParams: array of string): TManagerObjectSet; overload;
    {$ENDIF}
    function FindWhere<T: class, constructor>(const AWhere: string;
                                              const AOrderBy: string = ''): TManagerObjectSet;
    function NestedList<T: class>: TObjectList<T>;
    function ModifiedFields<T: class, constructor>: TDictionary<string, TList<string>>;
    function ExistSequence<T: class, constructor>: Boolean;
    function Insert<T: class, constructor>(const AObject: T): TManagerObjectSet;
    function Update<T: class, constructor>(const AObject: T): TManagerObjectSet;
    function Delete<T: class, constructor>(const AObject: T): TManagerObjectSet;
    function Modify<T: class, constructor>(const AObject: T): TManagerObjectSet;
    function LoadLazy<T: class, constructor>(const AOwner, AObject: TObject): TManagerObjectSet;
    function NextPacket<T: class, constructor>: TManagerObjectSet;
  end;

implementation

{ TManagerObjectSet }

constructor TManagerObjectSet.Create(const AConnection: {$IFDEF DRIVERRESTFUL}IRESTConnection);
                                                        {$ELSE}IDBConnection);
                                                        {$ENDIF}
begin
  FConnection := AConnection;
  FRepository := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
  FNestedList := TObjectDictionary<string, TObjectList<TObject>>.Create([doOwnsValues]);
end;

destructor TManagerObjectSet.Destroy;
begin
  FNestedList.Free;
  FRepository.Free;
  inherited;
end;

function TManagerObjectSet.NestedList<T>: TObjectList<T>;
var
  LClassName: String;
begin
  LClassName := TClass(T).ClassName;
  if FNestedList.ContainsKey(LClassName) then
  begin
    Result := TObjectList<T>(FNestedList.Items[LClassName]);
  end;
end;

function TManagerObjectSet.Delete<T>(const AObject: T): TManagerObjectSet;
begin
  Resolver<T>.Delete(AObject);
  Result := Self;
end;

function TManagerObjectSet.ExistSequence<T>: Boolean;
begin
  Result := Resolver<T>.ExistSequence;
end;

function TManagerObjectSet.Find<T>(const AID: Integer): T;
begin
  Result := Resolver<T>.Find(AID);
end;

function TManagerObjectSet.Find<T>(const AID: String): T;
begin
  Result := Resolver<T>.Find(AID);
end;

function TManagerObjectSet.AddAdapter<T>(const APageSize: Integer): TManagerObjectSet;
var
  LContainer: TObjectSetBaseAdapter<T>;
  LClassName: String;
begin
  LClassName := TClass(T).ClassName;
  if not FRepository.ContainsKey(LClassName) then
  begin
    {$IFDEF DRIVERRESTFUL}
    LContainer := TRESTObjectSetAdapter<T>.Create(FConnection, APageSize);
    {$ELSE}
    LContainer := TObjectSetAdapter<T>.Create(FConnection, APageSize);
    {$ENDIF}
    /// <summary> Adiciona o container ao repositório </summary>
    FRepository.Add(LClassName, LContainer);
  end;
  Result := Self;
end;

function TManagerObjectSet.Find<T>: TManagerObjectSet;
var
  LObjectList: TObjectList<T>;
begin
  LObjectList := Resolver<T>.Find;
  /// <summary> Limpa a lista de objectos </summary>
  FNestedList.AddOrSetValue(TClass(T).ClassName, TObjectList<TObject>(LObjectList));
  Result := Self;
end;

function TManagerObjectSet.Resolver<T>: TObjectSetBaseAdapter<T>;
var
  LClassName: String;
begin
  LClassName := TClass(T).ClassName;
  if FRepository.ContainsKey(LClassName) then
    Result := TObjectSetBaseAdapter<T>(FRepository.Items[LClassName]);
end;

function TManagerObjectSet.Update<T>(const AObject: T): TManagerObjectSet;
begin
  Resolver<T>.Update(AObject);
  Result := Self;
end;

function TManagerObjectSet.FindWhere<T>(const AWhere, AOrderBy: string): TManagerObjectSet;
var
  LObjectList: TObjectList<T>;
begin
  LObjectList := Resolver<T>.FindWhere(AWhere, AOrderBy);
  /// <summary> Limpa a lista de objectos </summary>
  FNestedList.AddOrSetValue(TClass(T).ClassName, TObjectList<TObject>(LObjectList));
  Result := Self;
end;

function TManagerObjectSet.Insert<T>(const AObject: T): TManagerObjectSet;
begin
  Resolver<T>.Insert(AObject);
  Result := Self;
end;

function TManagerObjectSet.LoadLazy<T>(const AOwner, AObject: TObject): TManagerObjectSet;
begin
  Resolver<T>.LoadLazy(AOwner, AObject);
  Result := Self;
end;

function TManagerObjectSet.ModifiedFields<T>: TDictionary<string, TList<string>>;
begin
  Result := Resolver<T>.ModifiedFields;
end;

function TManagerObjectSet.Modify<T>(const AObject: T): TManagerObjectSet;
begin
  Resolver<T>.Modify(AObject);
  Result := Self;
end;

function TManagerObjectSet.NextPacket<T>: TManagerObjectSet;
var
  LObjectList: TObjectList<T>;
begin
  LObjectList := TObjectList<T>(FNestedList.Items[TClass(T).ClassName]);
  Resolver<T>.NextPacket(LObjectList);
  Result := Self;
end;

{$IFDEF DRIVERRESTFUL}
function TManagerObjectSet.Find<T>(const AMethodName: String;
  const AParams: array of string): TManagerObjectSet;
var
  LObjectList: TObjectList<T>;
begin
  LObjectList := Resolver<T>.Find(AMethodName, AParams);
  /// <summary> Limpa a lista de objectos </summary>
  FNestedList.AddOrSetValue(TClass(T).ClassName, TObjectList<TObject>(LObjectList));
  Result := Self;
end;
{$ENDIF}

end.

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
  Rtti,
  Generics.Collections,
  /// ORMBr
  {$IFDEF DRIVERRESTFUL}
  ormbr.restobjectset.adapter,
  ormbr.client.interfaces,
  {$ELSE}
  ormbr.objectset.adapter,
  ormbr.factory.interfaces,
  {$ENDIF}
  {$IFDEF USEBINDSOURCE}
  ormbr.bind.source.interfaces,
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
    FCurrentIndex: Integer;
    FSelectedObject: TObject;
    {$IFDEF USEBINDSOURCE}
    FBindSourceObjectAdapter: IBindSourceObjectAdapter;
    {$ENDIF}
    function Resolver<T: class, constructor>: TObjectSetBaseAdapter<T>;
    procedure ListChanged<T: class, constructor>(Sender: TObject;
      const Item: T; Action: TCollectionNotification);
    procedure SelectNestedListItem<T: class>;
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
    function Find<T: class, constructor>(const AMethodName: String;
      const AParams: array of string): TManagerObjectSet; overload;
    {$ENDIF}
    function FindWhere<T: class, constructor>(const AWhere: string;
                                              const AOrderBy: string = ''): TManagerObjectSet;
    function NestedList<T: class>: TObjectList<T>;
    function ModifiedFields<T: class, constructor>: TDictionary<string, TList<string>>;
    function ExistSequence<T: class, constructor>: Boolean;
    function Insert<T: class, constructor>(const AObject: T): TManagerObjectSet; overload;
    function Update<T: class, constructor>(const AObject: T): TManagerObjectSet; overload;
    function Delete<T: class, constructor>(const AObject: T): TManagerObjectSet; overload;
    function Modify<T: class, constructor>(const AObject: T): TManagerObjectSet; overload;
    function LoadLazy<T: class, constructor>(const AOwner, AObject: TObject): TManagerObjectSet;
    function NextPacket<T: class, constructor>: TManagerObjectSet;
    function New<T: class, constructor>(var AObject: T): TManagerObjectSet; overload;
    {$IFDEF USEBINDSOURCE}
    function SetBindSourceObjectAdapter<T: class, constructor>(const ABindSourceObject: IBindSourceObjectAdapter): TManagerObjectSet;
    {$ENDIF}
    function Current<T: class, constructor>: T;
    function New<T: class, constructor>: TManagerObjectSet; overload;
    function Insert<T: class, constructor>: TManagerObjectSet; overload;
    function Modify<T: class, constructor>: TManagerObjectSet; overload;
    function Update<T: class, constructor>: TManagerObjectSet; overload;
    function Delete<T: class, constructor>: TManagerObjectSet; overload;
    function First<T: class, constructor>: TManagerObjectSet;
    function Next<T: class, constructor>: TManagerObjectSet;
    function Prior<T: class, constructor>: TManagerObjectSet;
    function Last<T: class, constructor>: TManagerObjectSet;
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
  FCurrentIndex := 0;
end;

function TManagerObjectSet.Current<T>: T;
begin
  Result := FNestedList.Items[T.ClassName].Items[FCurrentIndex] as T;
end;

function TManagerObjectSet.Delete<T>(const AObject: T): TManagerObjectSet;
begin
  Result := Self;
  Resolver<T>.Delete(AObject);
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
  Result := nil;
  LClassName := TClass(T).ClassName;
  if FNestedList.ContainsKey(LClassName) then
  begin
    Result := TObjectList<T>(FNestedList.Items[LClassName]);
  end;
end;

function TManagerObjectSet.New<T>: TManagerObjectSet;
var
  LNewObject: T;
begin
  Result := Self;
  Resolver<T>.New(LNewObject);
  FNestedList.Items[TClass(T).ClassName].Add(LNewObject);
  /// <summary>
  ///   O último passa a ser o objeto corrente.
  /// </summary>
  FCurrentIndex := FNestedList.Items[TClass(T).ClassName].Count -1;
end;

function TManagerObjectSet.New<T>(var AObject: T): TManagerObjectSet;
begin
  AObject := nil;
  Resolver<T>.New(AObject);
  Result := Self;
end;

function TManagerObjectSet.Delete<T>: TManagerObjectSet;
begin
  Result := Self;
  SelectNestedListItem<T>;
  Resolver<T>.Delete(FSelectedObject);
  FNestedList.Items[TClass(T).ClassName].Delete(FCurrentIndex);

  if FNestedList.Items[TClass(T).ClassName].Count > 0 then
    Dec(FCurrentIndex);
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
  Result := Self;
  LClassName := TClass(T).ClassName;
  if FRepository.ContainsKey(LClassName) then
    Exit;
  {$IFDEF DRIVERRESTFUL}
  LContainer := TRESTObjectSetAdapter<T>.Create(FConnection, APageSize);
  {$ELSE}
  LContainer := TObjectSetAdapter<T>.Create(FConnection, APageSize);
  {$ENDIF}
  /// <summary>
  ///   Adiciona o container ao repositório de containers
  /// </summary>
  FRepository.Add(LClassName, LContainer);
end;

function TManagerObjectSet.Find<T>: TManagerObjectSet;
var
  LObjectList: TObjectList<T>;
begin
  LObjectList := Resolver<T>.Find;
  LObjectList.OnNotify := ListChanged<T>;
  /// <summary>
  ///   Lista de objetos
  /// </summary>
  FNestedList.AddOrSetValue(TClass(T).ClassName, TObjectList<TObject>(LObjectList));
  Result := Self;
end;

function TManagerObjectSet.Resolver<T>: TObjectSetBaseAdapter<T>;
var
  LClassName: String;
begin
  Result := nil;
  LClassName := TClass(T).ClassName;
  if FRepository.ContainsKey(LClassName) then
    Result := TObjectSetBaseAdapter<T>(FRepository.Items[LClassName]);
end;

{$IFDEF USEBINDSOURCE}
function TManagerObjectSet.SetBindSourceObjectAdapter<T>(
  const ABindSourceObject: IBindSourceObjectAdapter): TManagerObjectSet;
begin
  FBindSourceObjectAdapter := ABindSourceObject;
  Resolver<T>.SetOnPropertyEvent(procedure(AProperty: TRttiProperty; AClassName: String)
                                 begin
                                   FBindSourceObjectAdapter
                                     .NotificationProperty(AProperty, AClassName);
                                 end);
  Resolver<T>.SetOnUpdateEvent(procedure(AObject: TObject)
                               begin
                                 FBindSourceObjectAdapter
                                   .BindNotification(AObject, cnExtracted);
                               end);
end;
{$ENDIF}

procedure TManagerObjectSet.SelectNestedListItem<T>;
begin
  FSelectedObject := FNestedList.Items[TClass(T).ClassName].Items[FCurrentIndex];
end;

function TManagerObjectSet.Update<T>: TManagerObjectSet;
begin
  Result := Self;
  SelectNestedListItem<T>;
  Resolver<T>.Update(FSelectedObject);
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
  LObjectList.OnNotify := ListChanged<T>;
  /// <summary>
  ///   Lista de objetos
  /// </summary>
  FNestedList.AddOrSetValue(TClass(T).ClassName, TObjectList<TObject>(LObjectList));
  Result := Self;
end;

function TManagerObjectSet.First<T>: TManagerObjectSet;
begin
  Result := Self;
  FCurrentIndex := 0;
end;

function TManagerObjectSet.Insert<T>: TManagerObjectSet;
begin
  Result := Self;
  New<T>;
end;

function TManagerObjectSet.Insert<T>(const AObject: T): TManagerObjectSet;
begin
  Result := Self;
  Resolver<T>.Insert(AObject);
end;

function TManagerObjectSet.Last<T>: TManagerObjectSet;
begin
  Result := Self;
  FCurrentIndex := FNestedList.Items[TClass(T).ClassName].Count -1;
end;

procedure TManagerObjectSet.ListChanged<T>(Sender: TObject; const Item: T;
  Action: TCollectionNotification);
begin
  if Action = cnAdding then // Before
  begin

  end
  else
  if Action = cnAdded then // After
  begin
    FCurrentIndex := FNestedList.Items[TClass(T).ClassName].Count -1;
  end
  else
  if Action = cnDeleting then // Before
  begin

  end
  else
  if Action = cnRemoved then // After
  begin

  end
  else
  if Action = cnExtracting then // Before
  begin

  end
  else
  if Action = cnExtracted then // After
  begin

  end;
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

function TManagerObjectSet.Modify<T>: TManagerObjectSet;
begin
  Result := Self;
  SelectNestedListItem<T>;
  Resolver<T>.Modify(FSelectedObject);
end;

function TManagerObjectSet.Modify<T>(const AObject: T): TManagerObjectSet;
begin
  Resolver<T>.Modify(AObject);
  Result := Self;
end;

function TManagerObjectSet.Next<T>: TManagerObjectSet;
begin
  Result := Self;
  if FCurrentIndex < FNestedList.Items[TClass(T).ClassName].Count -1 then
    Inc(FCurrentIndex);
end;

function TManagerObjectSet.NextPacket<T>: TManagerObjectSet;
var
  LObjectList: TObjectList<T>;
begin
  LObjectList := TObjectList<T>(FNestedList.Items[TClass(T).ClassName]);
  Resolver<T>.NextPacket(LObjectList);
  Result := Self;
end;

function TManagerObjectSet.Prior<T>: TManagerObjectSet;
begin
  Result := Self;
  if FCurrentIndex > FNestedList.Items[TClass(T).ClassName].Count -1 then
    Dec(FCurrentIndex);
end;

{$IFDEF DRIVERRESTFUL}
function TManagerObjectSet.Find<T>(const AMethodName: String;
  const AParams: array of string): TManagerObjectSet;
var
  LObjectList: TObjectList<T>;
begin
  LObjectList := Resolver<T>.Find(AMethodName, AParams);
  LObjectList.OnNotify := ListChanged<T>;
  /// <summary>
  ///   Lista de objetos
  /// </summary>
  FNestedList.AddOrSetValue(TClass(T).ClassName, TObjectList<TObject>(LObjectList));
  Result := Self;
end;
{$ENDIF}

end.

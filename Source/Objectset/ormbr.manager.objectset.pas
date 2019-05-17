{
      ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)

  ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi.
}

{$INCLUDE ..\ormbr.inc}

unit ormbr.manager.objectset;

interface

uses
  Rtti,
  Variants,
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
    FOwnerNestedList: Boolean;
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
    function Find<T: class, constructor>: TObjectList<T>; overload;
    function Find<T: class, constructor>(const AID: Variant): T; overload;
    {$IFDEF DRIVERRESTFUL}
    function Find<T: class, constructor>(const AMethodName: String;
      const AParams: array of string): TObjectList<T>; overload;
    {$ENDIF}
    function FindWhere<T: class, constructor>(const AWhere: string;
                                              const AOrderBy: string = ''): TObjectList<T>;
    function NestedList<T: class>: TObjectList<T>;
    function ModifiedFields<T: class, constructor>: TDictionary<string, TList<string>>;
    function ExistSequence<T: class, constructor>: Boolean;
    function Insert<T: class, constructor>(const AObject: T): Integer; overload;
    procedure Modify<T: class, constructor>(const AObject: T); overload;
    procedure Update<T: class, constructor>(const AObject: T); overload;
    procedure Delete<T: class, constructor>(const AObject: T); overload;
    procedure NextPacket<T: class, constructor>(var AObjectList: TObjectList<T>); overload;
    procedure New<T: class, constructor>(var AObject: T); overload;
    procedure LoadLazy<T: class, constructor>(const AOwner, AObject: TObject);
    {$IFDEF USEBINDSOURCE}
    function SetBindSourceObjectAdapter<T: class, constructor>(const ABindSourceObject: IBindSourceObjectAdapter): TManagerObjectSet;
    {$ENDIF}
    function Current<T: class, constructor>: T; overload;
    function Current<T: class, constructor>(const AIndex: Integer): T; overload;
    function New<T: class, constructor>: Integer; overload;
   	function Insert<T: class, constructor>: Integer; overload;
    procedure Modify<T: class, constructor>; overload;
    procedure Update<T: class, constructor>; overload;
    procedure Delete<T: class, constructor>; overload;
    procedure NextPacket<T: class, constructor>; overload;
    function First<T: class, constructor>: Integer;
    function Next<T: class, constructor>: Integer;
    function Prior<T: class, constructor>: Integer;
    function Last<T: class, constructor>: Integer;
    property OwnerNestedList: Boolean read FOwnerNestedList write FOwnerNestedList;
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
  FOwnerNestedList := True;
end;

function TManagerObjectSet.Current<T>(const AIndex: Integer): T;
begin
  FCurrentIndex := AIndex;
  Result := Current<T>;
end;

function TManagerObjectSet.Current<T>: T;
begin
  Result := FNestedList.Items[T.ClassName].Items[FCurrentIndex] as T;
end;

procedure TManagerObjectSet.Delete<T>(const AObject: T);
begin
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
    Result := TObjectList<T>(FNestedList.Items[LClassName]);
end;

function TManagerObjectSet.New<T>: Integer;
var
  LNewObject: T;
begin
  Result := FCurrentIndex;
  Resolver<T>.New(LNewObject);
  FNestedList.Items[TClass(T).ClassName].Add(LNewObject);
  /// <summary>
  ///   O �ltimo passa a ser o objeto corrente.
  /// </summary>
  FCurrentIndex := FNestedList.Items[TClass(T).ClassName].Count -1;
end;

procedure TManagerObjectSet.New<T>(var AObject: T);
begin
  AObject := nil;
  Resolver<T>.New(AObject);
end;
procedure TManagerObjectSet.Delete<T>;
begin
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

function TManagerObjectSet.Find<T>(const AID: Variant): T;
begin
  if TVarData(AID).VType = varInteger then
    Result := Resolver<T>.Find(Integer(AID))
  else
  if TVarData(AID).VType = varString then
    Result := Resolver<T>.Find(VarToStr(AID))
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
  ///   Adiciona o container ao reposit�rio de containers
  /// </summary>
  FRepository.Add(LClassName, LContainer);
end;

function TManagerObjectSet.Find<T>: TObjectList<T>;
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
  LObjectList.OnNotify := ListChanged<T>;
  /// <summary>
  ///   Lista de objetos
  /// </summary>
  FNestedList.AddOrSetValue(TClass(T).ClassName, TObjectList<TObject>(LObjectList));
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

procedure TManagerObjectSet.Update<T>;
begin
  SelectNestedListItem<T>;
  Resolver<T>.Update(FSelectedObject);
end;

procedure TManagerObjectSet.Update<T>(const AObject: T);
begin
  Resolver<T>.Update(AObject);
end;

function TManagerObjectSet.FindWhere<T>(const AWhere, AOrderBy: string): TObjectList<T>;
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
  LObjectList.OnNotify := ListChanged<T>;
  /// <summary>
  ///   Lista de objetos
  /// </summary>
  FNestedList.AddOrSetValue(TClass(T).ClassName, TObjectList<TObject>(LObjectList));
end;

function TManagerObjectSet.First<T>: Integer;
begin
  Result := FCurrentIndex;
  FCurrentIndex := 0;
end;

function TManagerObjectSet.Insert<T>: Integer;
var
  LNewObject: T;
begin
  Result := FCurrentIndex;
  Resolver<T>.New(LNewObject);
  FNestedList.Items[TClass(T).ClassName].Add(LNewObject);
  /// <summary>
  ///   O �ltimo passa a ser o objeto corrente.
  /// </summary>
  FCurrentIndex := FNestedList.Items[TClass(T).ClassName].Count -1;
end;

function TManagerObjectSet.Insert<T>(const AObject: T): Integer;
begin
  Result := FCurrentIndex;
  Resolver<T>.Insert(AObject);
  FNestedList.Items[TClass(T).ClassName].Add(AObject);
  /// <summary>
  ///   O �ltimo passa a ser o objeto corrente.
  /// </summary>
  FCurrentIndex := FNestedList.Items[TClass(T).ClassName].Count -1;
end;

function TManagerObjectSet.Last<T>: Integer;
begin
  Result := FCurrentIndex;
  FCurrentIndex := FNestedList.Items[TClass(T).ClassName].Count -1;
end;

procedure TManagerObjectSet.ListChanged<T>(Sender: TObject; const Item: T;
  Action: TCollectionNotification);
var
  LClassName: String;
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
    LClassName := TClass(T).ClassName;
    if not FNestedList.ContainsKey(LClassName) then
      Exit;

    if FNestedList.Items[LClassName].Count = 0 then
      Dec(FCurrentIndex);

    if FNestedList.Items[LClassName].Count > 1 then
      Dec(FCurrentIndex);
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

procedure TManagerObjectSet.LoadLazy<T>(const AOwner, AObject: TObject);
begin
  Resolver<T>.LoadLazy(AOwner, AObject);
end;

function TManagerObjectSet.ModifiedFields<T>: TDictionary<string, TList<string>>;
begin
  Result := Resolver<T>.ModifiedFields;
end;

procedure TManagerObjectSet.Modify<T>;
begin
  SelectNestedListItem<T>;
  Resolver<T>.Modify(FSelectedObject);
end;

procedure TManagerObjectSet.Modify<T>(const AObject: T);
begin
  Resolver<T>.Modify(AObject);
end;

function TManagerObjectSet.Next<T>: Integer;
begin
  Result := FCurrentIndex;
  if FCurrentIndex < FNestedList.Items[TClass(T).ClassName].Count -1 then
    Inc(FCurrentIndex);
end;

procedure TManagerObjectSet.NextPacket<T>(var AObjectList: TObjectList<T>);
begin
  Resolver<T>.NextPacket(AObjectList);
end;

procedure TManagerObjectSet.NextPacket<T>;
var
  LObjectList: TObjectList<T>;
begin
  LObjectList := TObjectList<T>(FNestedList.Items[TClass(T).ClassName]);
  Resolver<T>.NextPacket(LObjectList);
end;

function TManagerObjectSet.Prior<T>: Integer;
begin
  Result := FCurrentIndex;
  if FCurrentIndex > FNestedList.Items[TClass(T).ClassName].Count -1 then
    Dec(FCurrentIndex);
end;

{$IFDEF DRIVERRESTFUL}
function TManagerObjectSet.Find<T>(const AMethodName: String;
  const AParams: array of string): TObjectList<T>;
var
  LObjectList: TObjectList<T>;
begin
  Result := nil;
  if not FOwnerNestedList then
  begin
    Result := Resolver<T>.Find(AMethodName, AParams);
    Exit;
  end;
  LObjectList := Resolver<T>.Find(AMethodName, AParams);
  LObjectList.OnNotify := ListChanged<T>;
  /// <summary>
  ///   Lista de objetos
  /// </summary>
  FNestedList.AddOrSetValue(TClass(T).ClassName, TObjectList<TObject>(LObjectList));
end;
{$ENDIF}

end.

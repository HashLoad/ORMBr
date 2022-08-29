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
  SysUtils,
  Variants,
  Generics.Collections,
  // ORMBr
  {$IFDEF DRIVERRESTFUL}
    ormbr.restobjectset.adapter,
    ormbr.client.interfaces,
  {$ELSE}
    ormbr.objectset.adapter,
    dbebr.factory.interfaces,
  {$ENDIF}
  ormbr.objectset.base.adapter;

type
  TRepository = class
  private
    FObjectSet: TObject;
    FNestedList: TObjectList<TObject>;
  public
    constructor Create;
    destructor Destroy; override;
    property ObjectSet: TObject read FObjectSet write FObjectSet;
    property NestedList: TObjectList<TObject> read FNestedList write FNestedList;
  end;
  // Lista de Container
  TRepositoryList = TObjectDictionary<string, TRepository>;
  IMOConnection = {$IFDEF DRIVERRESTFUL}IRESTConnection{$ELSE}IDBConnection{$ENDIF};

  // TManagerObjectSet
  TManagerObjectSet = class
  private
    FConnection: IMOConnection;
    FRepository: TRepositoryList;
    FCurrentIndex: Integer;
    FSelectedObject: TObject;
    FOwnerNestedList: Boolean;
    function Resolver<T: class, constructor>: TObjectSetBaseAdapter<T>;
    procedure ListChanged<T: class, constructor>(Sender: TObject;
      const Item: T; Action: TCollectionNotification);
    procedure SelectNestedListItem<T: class>;
    procedure LoadLazy<T: class, constructor>(const AOwner, AObject: TObject); overload;
  public
    constructor Create(const AConnection: IMOConnection);
    destructor Destroy; override;
    function AddAdapter<T: class, constructor>(const APageSize: Integer = -1): TManagerObjectSet; deprecated 'Use AddRepository()';
    function AddRepository<T: class, constructor>(const APageSize: Integer = -1): TManagerObjectSet;
    function NestedList<T: class>: TObjectList<T>;
    // ObjectSet
    function Find<T: class, constructor>: TObjectList<T>; overload;
    function Find<T: class, constructor>(const AID: Variant): T; overload;
    function FindWhere<T: class, constructor>(const AWhere: string;
                                              const AOrderBy: string = ''): TObjectList<T>;
    {$IFDEF DRIVERRESTFUL}
    function Find<T: class, constructor>(const AMethodName: String;
      const AParams: array of string): TObjectList<T>; overload;
    {$ENDIF}
    function ModifiedFields<T: class, constructor>: TDictionary<string, TDictionary<string, string>>;
    function ExistSequence<T: class, constructor>: Boolean;
    procedure LoadLazy<T: class, constructor>(const AObject: TObject); overload;
    // Métodos para serem usados com a propriedade OwnerNestedList := False;
    function Insert<T: class, constructor>(const AObject: T): Integer; overload;
    procedure Modify<T: class, constructor>(const AObject: T); overload;
    procedure Update<T: class, constructor>(const AObject: T); overload;
    procedure Delete<T: class, constructor>(const AObject: T); overload;
    procedure NextPacket<T: class, constructor>(var AObjectList: TObjectList<T>); overload;
    procedure New<T: class, constructor>(var AObject: T); overload;
    // Métodos para serem usados com a propriedade OwnerNestedList := True;
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
    function Eof<T: class>: Boolean;
    function Bof<T: class>: Boolean;
    property OwnerNestedList: Boolean read FOwnerNestedList write FOwnerNestedList;
  end;

implementation

{ TManagerObjectSet }

function TManagerObjectSet.AddRepository<T>(
  const APageSize: Integer): TManagerObjectSet;
var
  LObjectetAdapter: TObjectSetBaseAdapter<T>;
  LClassName: String;
  LRepository: TRepository;
begin
  Result := Self;
  LClassName := TClass(T).ClassName;
  if FRepository.ContainsKey(LClassName) then
    Exit;

  {$IFDEF DRIVERRESTFUL}
    LObjectetAdapter := TRESTObjectSetAdapter<T>.Create(FConnection, APageSize);
  {$ELSE}
    LObjectetAdapter := TObjectSetAdapter<T>.Create(FConnection, APageSize);
  {$ENDIF}
  // Adiciona o container ao repositório de containers
  LRepository := TRepository.Create;
  LRepository.ObjectSet := LObjectetAdapter;
  FRepository.Add(LClassName, LRepository);
end;

function TManagerObjectSet.Bof<T>: Boolean;
begin
  Result := False;
  if not FOwnerNestedList then
    Exit;
  Result := (FCurrentIndex = FRepository.Items[T.ClassName].NestedList.Count -1);
end;

constructor TManagerObjectSet.Create(const AConnection: IMOConnection);
begin
  FConnection := AConnection;
  FRepository := TRepositoryList.Create([doOwnsValues]);
  FCurrentIndex := 0;
  FOwnerNestedList := False;
end;

function TManagerObjectSet.Current<T>(const AIndex: Integer): T;
begin
  Result := nil;
  if not FOwnerNestedList then
    Exit;
  FCurrentIndex := AIndex;
  Result := Current<T>;
end;

function TManagerObjectSet.Current<T>: T;
begin
  Result := nil;
  if not FOwnerNestedList then
    Exit;
  SelectNestedListItem<T>;
  Result := T(FRepository.Items[T.ClassName].NestedList.Items[FCurrentIndex]);
end;

procedure TManagerObjectSet.Delete<T>(const AObject: T);
begin
  Resolver<T>.Delete(AObject);
end;

destructor TManagerObjectSet.Destroy;
begin
  FRepository.Free;
  inherited;
end;

function TManagerObjectSet.NestedList<T>: TObjectList<T>;
var
  LClassName: String;
begin
  Result := nil;
  if not FOwnerNestedList then
    raise Exception.Create('Enable the OwnerNestedList property');
  LClassName := TClass(T).ClassName;
  if FRepository.ContainsKey(LClassName) then
    Result := TObjectList<T>(FRepository.Items[LClassName].NestedList);
end;

function TManagerObjectSet.New<T>: Integer;
var
  LNewObject: T;
  LObjectList: TObjectList<T>;
begin
  Result := -1;
  if not FOwnerNestedList then
    Exit;
  if not Assigned(FRepository.Items[TClass(T).ClassName].NestedList) then
  begin
    LObjectList := TObjectList<T>.Create;
    LObjectList.OnNotify := ListChanged<T>;
    FRepository.Items[TClass(T).ClassName].NestedList := TObjectList<TObject>(LObjectList);
    FCurrentIndex := 0;
  end;
  Resolver<T>.New(LNewObject);
  FRepository.Items[TClass(T).ClassName].NestedList.Add(LNewObject);
  FCurrentIndex := FRepository.Items[TClass(T).ClassName].NestedList.Count -1;
  Result := FCurrentIndex;
end;

procedure TManagerObjectSet.New<T>(var AObject: T);
begin
  AObject := nil;
  Resolver<T>.New(AObject);
end;
procedure TManagerObjectSet.Delete<T>;
begin
  if not FOwnerNestedList then
    Exit;
  SelectNestedListItem<T>;
  Resolver<T>.Delete(FSelectedObject);
  FRepository.Items[TClass(T).ClassName].NestedList.Delete(FCurrentIndex);
end;

function TManagerObjectSet.Eof<T>: Boolean;
begin
  Result := False;
  if not FOwnerNestedList then
    Exit;
  Result := (FCurrentIndex +1 > FRepository.Items[T.ClassName].NestedList.Count -1);
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
    Result := Resolver<T>.Find(VarToStr(AID));
  FCurrentIndex := 0;
end;

function TManagerObjectSet.AddAdapter<T>(const APageSize: Integer): TManagerObjectSet;
begin
  Result := AddRepository<T>(APageSize);
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
  // Lista de objetos
  FRepository.Items[TClass(T).ClassName].NestedList.Free;
  FRepository.Items[TClass(T).ClassName].NestedList := TObjectList<TObject>(LObjectList);
  FCurrentIndex := 0;
end;

function TManagerObjectSet.Resolver<T>: TObjectSetBaseAdapter<T>;
var
  LClassName: String;
begin
  Result := nil;
  LClassName := TClass(T).ClassName;
  if not FRepository.ContainsKey(LClassName) then
    raise Exception.Create('Use the AddAdapter<T> method to add the class to manager');
  Result := TObjectSetBaseAdapter<T>(FRepository.Items[LClassName].ObjectSet);
end;

procedure TManagerObjectSet.SelectNestedListItem<T>;
begin
  FSelectedObject := FRepository.Items[TClass(T).ClassName].NestedList.Items[FCurrentIndex];
end;

procedure TManagerObjectSet.Update<T>;
begin
  if not FOwnerNestedList then
    Exit;
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
  // Lista de objetos
  FRepository.Items[TClass(T).ClassName].NestedList.Free;
  FRepository.Items[TClass(T).ClassName].NestedList := TObjectList<TObject>(LObjectList);
  FCurrentIndex := 0;
end;

function TManagerObjectSet.First<T>: Integer;
begin
  Result := -1;
  if not FOwnerNestedList then
    Exit;
  FCurrentIndex := 0;
  Result := FCurrentIndex;
end;

function TManagerObjectSet.Insert<T>: Integer;
begin
  Result := -1;
  if not FOwnerNestedList then
    Exit;
  Resolver<T>.Insert(FSelectedObject);
  Result := FCurrentIndex;
end;

function TManagerObjectSet.Insert<T>(const AObject: T): Integer;
begin
  Result := FCurrentIndex;
  Resolver<T>.Insert(AObject);
  if not FOwnerNestedList then
    Exit;
  FRepository.Items[TClass(T).ClassName].NestedList.Add(AObject);
  FCurrentIndex := FRepository.Items[TClass(T).ClassName].NestedList.Count -1;
end;

function TManagerObjectSet.Last<T>: Integer;
begin
  Result := -1;
  if not FOwnerNestedList then
    Exit;
  FCurrentIndex := FRepository.Items[TClass(T).ClassName].NestedList.Count -1;
  Result := FCurrentIndex;
end;

procedure TManagerObjectSet.ListChanged<T>(Sender: TObject; const Item: T;
  Action: TCollectionNotification);
var
  LClassName: String;
begin
  if Action = cnAdded then // After
  begin
    FCurrentIndex := FRepository.Items[TClass(T).ClassName].NestedList.Count -1;
  end
  else
  if Action = cnRemoved then // After
  begin
    LClassName := TClass(T).ClassName;
    if not FRepository.ContainsKey(LClassName) then
      Exit;

    if FRepository.Items[LClassName].NestedList.Count = 0 then
      Dec(FCurrentIndex);

    if FRepository.Items[LClassName].NestedList.Count > 1 then
      Dec(FCurrentIndex);
  end
  else
  if Action = cnExtracted then // After
  begin

  end;
end;

procedure TManagerObjectSet.LoadLazy<T>(const AObject: TObject);
begin
  Resolver<T>.LoadLazy(TObjectSetBaseAdapter<T>(FRepository.Items[T.ClassName].ObjectSet), AObject);
end;

procedure TManagerObjectSet.LoadLazy<T>(const AOwner, AObject: TObject);
begin
  Resolver<T>.LoadLazy(AOwner, AObject);
end;

function TManagerObjectSet.ModifiedFields<T>: TDictionary<string, TDictionary<string, string>>;
begin
  Result := Resolver<T>.ModifiedFields;
end;

procedure TManagerObjectSet.Modify<T>;
begin
  if not FOwnerNestedList then
    Exit;
  SelectNestedListItem<T>;
  Resolver<T>.Modify(FSelectedObject);
end;

procedure TManagerObjectSet.Modify<T>(const AObject: T);
begin
  Resolver<T>.Modify(AObject);
end;

function TManagerObjectSet.Next<T>: Integer;
begin
  Result := -1;
  if not FOwnerNestedList then
    Exit;
  if FCurrentIndex < FRepository.Items[TClass(T).ClassName].NestedList.Count -1 then
    Inc(FCurrentIndex);
  Result := FCurrentIndex;
end;

procedure TManagerObjectSet.NextPacket<T>(var AObjectList: TObjectList<T>);
begin
  Resolver<T>.NextPacket(AObjectList);
end;

procedure TManagerObjectSet.NextPacket<T>;
var
  LObjectList: TObjectList<T>;
begin
  if not FOwnerNestedList then
    Exit;
  LObjectList := TObjectList<T>(FRepository.Items[TClass(T).ClassName].NestedList);
  Resolver<T>.NextPacket(LObjectList);
end;

function TManagerObjectSet.Prior<T>: Integer;
begin
  Result := -1;
  if not FOwnerNestedList then
    Exit;
  if FCurrentIndex > FRepository.Items[TClass(T).ClassName].NestedList.Count -1 then
    Dec(FCurrentIndex);
  Result := FCurrentIndex;
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
  // Lista de objetos
  FRepository.Items[TClass(T).ClassName].NestedList.Free;
  FRepository.Items[TClass(T).ClassName].NestedList := TObjectList<TObject>(LObjectList);
end;
{$ENDIF}

{ TRepository }

constructor TRepository.Create;
begin

end;

destructor TRepository.Destroy;
begin
  if Assigned(FObjectSet) then
    FObjectSet.Free;
  if Assigned(FNestedList) then
    FNestedList.Free;
  inherited;
end;

end.

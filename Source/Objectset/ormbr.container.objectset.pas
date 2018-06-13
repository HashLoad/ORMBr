unit ormbr.container.objectset;

interface

uses
  Rtti,
  Generics.Collections,
  /// ormbr
  ormbr.container.objectset.interfaces,
  ormbr.factory.interfaces,
  ormbr.objectset.adapter;

type
  TContainerObjectSet<M: class, constructor> = class(TInterfacedObject, IContainerObjectSet<M>)
  private
  protected
    FObjectSetAdapter: TObjectSetAdapter<M>;
    FConnection: IDBConnection;
  public
    constructor Create(const AConnection: IDBConnection; const APageSize: Integer = -1);
    destructor Destroy; override;
    function ExistSequence: Boolean;
    function ModifiedFields: TDictionary<string, TList<string>>;
    function Find: TObjectList<M>; overload;
    function Find(const AID: Integer): M; overload;
    function Find(const AID: String): M; overload;
    function FindWhere(const AWhere: string; const AOrderBy: string = ''): TObjectList<M>;
    procedure Insert(const AObject: M);
    procedure Update(const AObject: M);
    procedure Delete(const AObject: M);
    procedure Modify(const AObject: M);
    procedure LoadLazy(const AOwner, AObject: TObject);
    procedure NextPacket(const AObjectList: TObjectList<M>); virtual;
  end;

implementation

{ TContainerObjectSet<M> }

constructor TContainerObjectSet<M>.Create(const AConnection: IDBConnection;
  const APageSize: Integer);
begin
  FConnection := AConnection;
  FObjectSetAdapter := TObjectSetAdapter<M>.Create(AConnection, APageSize);
end;

destructor TContainerObjectSet<M>.Destroy;
begin
  FObjectSetAdapter.Free;
  inherited;
end;

procedure TContainerObjectSet<M>.Delete(const AObject: M);
begin
  inherited;
  FObjectSetAdapter.Delete(AObject);
end;

function TContainerObjectSet<M>.ExistSequence: Boolean;
begin
  inherited;
  Result := FObjectSetAdapter.ExistSequence;
end;

function TContainerObjectSet<M>.Find(const AID: String): M;
begin
  Result := FObjectSetAdapter.Find(AID);
end;

function TContainerObjectSet<M>.FindWhere(const AWhere, AOrderBy: string): TObjectList<M>;
begin
  inherited;
  Result := FObjectSetAdapter.FindWhere(AWhere, AOrderBy);
end;

function TContainerObjectSet<M>.Find(const AID: Integer): M;
begin
  inherited;
  Result := FObjectSetAdapter.Find(AID);
end;

function TContainerObjectSet<M>.Find: TObjectList<M>;
begin
  inherited;
  Result := FObjectSetAdapter.Find;
end;

procedure TContainerObjectSet<M>.Insert(const AObject: M);
begin
  inherited;
  FObjectSetAdapter.Insert(AObject);
end;

procedure TContainerObjectSet<M>.LoadLazy(const AOwner, AObject: TObject);
begin
  FObjectSetAdapter.LoadLazy(AOwner, AObject);
end;

function TContainerObjectSet<M>.ModifiedFields: TDictionary<string, TList<string>>;
begin
  Result := FObjectSetAdapter.ModifiedFields;
end;

procedure TContainerObjectSet<M>.Modify(const AObject: M);
begin
  inherited;
  FObjectSetAdapter.Modify(AObject);
end;

procedure TContainerObjectSet<M>.NextPacket(const AObjectList: TObjectList<M>);
begin
  inherited;
  FObjectSetAdapter.NextPacket(AObjectList);
end;

procedure TContainerObjectSet<M>.Update(const AObject: M);
begin
  inherited;
  FObjectSetAdapter.Update(AObject);
end;

end.

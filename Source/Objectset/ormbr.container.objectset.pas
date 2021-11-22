unit ormbr.container.objectset;

interface

uses
  Rtti,
  Generics.Collections,
  /// ormbr
  ormbr.container.objectset.interfaces,
  dbebr.factory.interfaces,
  ormbr.objectset.adapter;

type
  TContainerObjectSet<M: class, constructor> = class(TInterfacedObject, IContainerObjectSet<M>)
  protected
    FObjectSetAdapter: TObjectSetAdapter<M>;
  public
    constructor Create(const AConnection: IDBConnection; const APageSize: Integer = -1);
    destructor Destroy; override;
    function ExistSequence: Boolean;
    function ModifiedFields: TDictionary<string, TDictionary<string, string>>;
    function Find: TObjectList<M>; overload;
    function Find(const AID: Int64): M; overload;
    function Find(const AID: String): M; overload;
    function FindWhere(const AWhere: string; const AOrderBy: string = ''): TObjectList<M>;
    procedure Insert(const AObject: M);
    procedure Update(const AObject: M);
    procedure Delete(const AObject: M);
    procedure Modify(const AObject: M);
    procedure LoadLazy(const AOwner, AObject: TObject);
    procedure NextPacket(const AObjectList: TObjectList<M>); overload; virtual;
    function NextPacket: TObjectList<M>; overload; virtual;
    function NextPacket(const APageSize, APageNext: Integer): TObjectList<M>; overload; virtual;
    function NextPacket(const AWhere, AOrderBy: String; const APageSize, APageNext: Integer): TObjectList<M>; overload; virtual;
  end;

implementation

{ TContainerObjectSet<M> }

constructor TContainerObjectSet<M>.Create(const AConnection: IDBConnection;
  const APageSize: Integer);
begin
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

function TContainerObjectSet<M>.Find(const AID: Int64): M;
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

function TContainerObjectSet<M>.ModifiedFields: TDictionary<string, TDictionary<string, string>>;
begin
  Result := FObjectSetAdapter.ModifiedFields;
end;

procedure TContainerObjectSet<M>.Modify(const AObject: M);
begin
  inherited;
  FObjectSetAdapter.Modify(AObject);
end;

function TContainerObjectSet<M>.NextPacket(const APageSize, APageNext: Integer): TObjectList<M>;
begin
  Result := FObjectSetAdapter.NextPacket(APageSize, APageNext);
end;

function TContainerObjectSet<M>.NextPacket(const AWhere, AOrderBy: String;
  const APageSize, APageNext: Integer): TObjectList<M>;
begin
  Result := FObjectSetAdapter.NextPacket(AWhere, AOrderBy, APageSize, APageNext);
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

function TContainerObjectSet<M>.NextPacket: TObjectList<M>;
begin
  Result := FObjectSetAdapter.NextPacket;
end;

end.

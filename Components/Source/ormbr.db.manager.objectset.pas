unit ormbr.db.manager.objectset;

interface

uses
  DB,
  Classes,
  Generics.Collections,
  dbebr.connection.base,
  ormbr.manager.objectset;

type
  TORMBrManagerObjectSet = class(TComponent)
  private
    FOwner: TComponent;
    FConnection: TDBEBrConnectionBase;
    FManagerObjectSet: TManagerObjectSet;
    function GetConnection: TDBEBrConnectionBase;
    procedure SetConnection(const Value: TDBEBrConnectionBase);
    function GetOwnerNestedList: Boolean;
    procedure SetOwnerNestedList(const Value: Boolean);
  public
    constructor Create(const AOwner: TComponent);
    destructor Destroy; override;
    function AddAdapter<T: class, constructor>(const APageSize: Integer = -1): TManagerObjectSet;
    function NestedList<T: class>: TObjectList<T>;
    /// ObjectSet
    function Find<T: class, constructor>: TObjectList<T>; overload;
    function Find<T: class, constructor>(const AID: Variant): T; overload;
    function FindWhere<T: class, constructor>(const AWhere: string;
                                              const AOrderBy: string = ''): TObjectList<T>;
    function ModifiedFields<T: class, constructor>: TDictionary<string, TDictionary<string, string>>;
    function ExistSequence<T: class, constructor>: Boolean;
    procedure LoadLazy<T: class, constructor>(const AObject: TObject); overload;
    /// <summary>
    ///   Métodos para serem usados com a propriedade OwnerNestedList := False;
    /// </summary>
    function Insert<T: class, constructor>(const AObject: T): Integer; overload;
    procedure Modify<T: class, constructor>(const AObject: T); overload;
    procedure Update<T: class, constructor>(const AObject: T); overload;
    procedure Delete<T: class, constructor>(const AObject: T); overload;
    procedure NextPacket<T: class, constructor>(var AObjectList: TObjectList<T>); overload;
    procedure New<T: class, constructor>(var AObject: T); overload;
    /// <summary>
    ///   Métodos para serem usados com a propriedade OwnerNestedList := True;
    /// </summary>
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
    property OwnerNestedList: Boolean read GetOwnerNestedList write SetOwnerNestedList;
  published
    property Connection: TDBEBrConnectionBase read GetConnection write SetConnection;
  end;

implementation

function TORMBrManagerObjectSet.AddAdapter<T>(const APageSize: Integer): TManagerObjectSet;
begin
  Result := FManagerObjectSet.AddAdapter<T>(APageSize);
end;

constructor TORMBrManagerObjectSet.Create(const AOwner: TComponent);
begin
  FOwner := AOwner;
  OwnerNestedList := True;
end;

function TORMBrManagerObjectSet.Current<T>(const AIndex: Integer): T;
begin
  Result := FManagerObjectSet.Current<T>(AIndex);
end;

function TORMBrManagerObjectSet.Current<T>: T;
begin
  Result := FManagerObjectSet.Current<T>;
end;

procedure TORMBrManagerObjectSet.Delete<T>;
begin
  FManagerObjectSet.Delete<T>;
end;

procedure TORMBrManagerObjectSet.Delete<T>(const AObject: T);
begin
  FManagerObjectSet.Delete<T>(AObject);
end;

destructor TORMBrManagerObjectSet.Destroy;
begin
  if Assigned(FManagerObjectSet) then
    FManagerObjectSet.Free;
  inherited;
end;

function TORMBrManagerObjectSet.ExistSequence<T>: Boolean;
begin
  Result := FManagerObjectSet.ExistSequence<T>;
end;

function TORMBrManagerObjectSet.Find<T>: TObjectList<T>;
begin
  Result := FManagerObjectSet.Find<T>;
end;

function TORMBrManagerObjectSet.Find<T>(const AID: Variant): T;
begin
  Result := FManagerObjectSet.Find<T>(AID);
end;

function TORMBrManagerObjectSet.FindWhere<T>(const AWhere, AOrderBy: string): TObjectList<T>;
begin
  Result := FManagerObjectSet.FindWhere<T>(AWhere, AOrderBy);
end;

function TORMBrManagerObjectSet.First<T>: Integer;
begin
  Result := FManagerObjectSet.First<T>;
end;

function TORMBrManagerObjectSet.GetConnection: TDBEBrConnectionBase;
begin
  Result := FConnection;
end;

function TORMBrManagerObjectSet.GetOwnerNestedList: Boolean;
begin
  Result := FManagerObjectSet.OwnerNestedList;
end;

function TORMBrManagerObjectSet.Insert<T>(const AObject: T): Integer;
begin
  Result := FManagerObjectSet.Insert<T>(AObject);
end;

function TORMBrManagerObjectSet.Insert<T>: Integer;
begin
  Result := FManagerObjectSet.Insert<T>;
end;

function TORMBrManagerObjectSet.Last<T>: Integer;
begin
  Result := FManagerObjectSet.Last<T>;
end;

procedure TORMBrManagerObjectSet.LoadLazy<T>(const AObject: TObject);
begin
  FManagerObjectSet.LoadLazy<T>(AObject);
end;

function TORMBrManagerObjectSet.ModifiedFields<T>: TDictionary<string, TDictionary<string, string>>;
begin
  Result := FManagerObjectSet.ModifiedFields<T>;
end;

procedure TORMBrManagerObjectSet.Modify<T>;
begin
  FManagerObjectSet.Modify<T>;
end;

procedure TORMBrManagerObjectSet.Modify<T>(const AObject: T);
begin
  FManagerObjectSet.Modify<T>(AObject);
end;

function TORMBrManagerObjectSet.NestedList<T>: TObjectList<T>;
begin
  Result := FManagerObjectSet.NestedList<T>;
end;

procedure TORMBrManagerObjectSet.New<T>(var AObject: T);
begin
  FManagerObjectSet.New<T>(AObject);
end;

function TORMBrManagerObjectSet.New<T>: Integer;
begin
  Result := FManagerObjectSet.New<T>;
end;

function TORMBrManagerObjectSet.Next<T>: Integer;
begin
  Result := FManagerObjectSet.Next<T>;
end;

procedure TORMBrManagerObjectSet.NextPacket<T>(var AObjectList: TObjectList<T>);
begin
  FManagerObjectSet.NextPacket<T>(AObjectList);
end;

procedure TORMBrManagerObjectSet.NextPacket<T>;
begin
  FManagerObjectSet.NextPacket<T>;
end;

function TORMBrManagerObjectSet.Prior<T>: Integer;
begin
  Result := FManagerObjectSet.Prior<T>;
end;

procedure TORMBrManagerObjectSet.SetConnection(const Value: TDBEBrConnectionBase);
begin
  FConnection := Value;
  if Assigned(FManagerObjectSet) then
    FManagerObjectSet.Free;
  FManagerObjectSet := TManagerObjectSet.Create(FConnection.Connection);
end;

procedure TORMBrManagerObjectSet.SetOwnerNestedList(const Value: Boolean);
begin
  FManagerObjectSet.OwnerNestedList := Value;
end;

procedure TORMBrManagerObjectSet.Update<T>(const AObject: T);
begin
  FManagerObjectSet.Update<T>(AObject);
end;

procedure TORMBrManagerObjectSet.Update<T>;
begin
  FManagerObjectSet.Update<T>;
end;

end.

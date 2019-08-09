unit ormbr.db.manager.dataset;

interface

uses
  DB,
  Classes,
  Generics.Collections,
  dbebr.connection.base,
  ormbr.manager.dataset;

type
  TORMBrManagerDataSet = class(TComponent)
  private
    FOwner: TComponent;
    FConnection: TDBEBrConnectionBase;
    FManagerDataSet: TManagerDataSet;
    function GetConnection: TDBEBrConnectionBase;
    procedure SetConnection(const Value: TDBEBrConnectionBase);
    function GetOwnerNestedList: Boolean;
    procedure SetOwnerNestedList(const Value: Boolean);
  public
    procedure RemoveAdapter<T: class>;
    function AddAdapter<T: class, constructor>(ADataSet: TDataSet;
      const APageSize: Integer = -1): TManagerDataSet; overload;
    function AddAdapter<T, M: class, constructor>(ADataSet: TDataSet): TManagerDataSet; overload;
    function AddLookupField<T, M: class, constructor>(const AFieldName: string;
                                                      const AKeyFields: string;
                                                      const ALookupKeyFields: string;
                                                      const ALookupResultField: string;
                                                      const ADisplayLabel: string = ''): TManagerDataSet;
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
    function Find<T: class, constructor>(const AID: Variant): T; overload;
    function FindWhere<T: class, constructor>(const AWhere: string;
                                              const AOrderBy: string = ''): TObjectList<T>;
    function NestedList<T: class>: TObjectList<T>;
    function AutoNextPacket<T: class, constructor>(const AValue: Boolean): TManagerDataSet;
    property OwnerNestedList: Boolean read GetOwnerNestedList write SetOwnerNestedList;
  published
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    property Connection: TDBEBrConnectionBase read GetConnection write SetConnection;
  end;

implementation

{ TDBManagerDataSet }

function TORMBrManagerDataSet.AddAdapter<T, M>(ADataSet: TDataSet): TManagerDataSet;
begin
  Result := FManagerDataSet.AddAdapter<T, M>(ADataSet);
end;

function TORMBrManagerDataSet.AddAdapter<T>(ADataSet: TDataSet;
  const APageSize: Integer): TManagerDataSet;
begin
  Result := FManagerDataSet.AddAdapter<T>(ADataSet, APageSize);
end;

function TORMBrManagerDataSet.AddLookupField<T, M>(const AFieldName, AKeyFields,
  ALookupKeyFields, ALookupResultField, ADisplayLabel: string): TManagerDataSet;
begin
  Result := FManagerDataSet.AddLookupField<T, M>(AFieldName, AKeyFields,
                                                 ALookupKeyFields, ALookupResultField, ADisplayLabel);
end;

procedure TORMBrManagerDataSet.ApplyUpdates<T>(const MaxErros: Integer);
begin
  FManagerDataSet.ApplyUpdates<T>(MaxErros);
end;

function TORMBrManagerDataSet.AutoNextPacket<T>(const AValue: Boolean): TManagerDataSet;
begin
  Result := FManagerDataSet.AutoNextPacket<T>(AValue);
end;

procedure TORMBrManagerDataSet.CancelUpdates<T>;
begin
  FManagerDataSet.CancelUpdates<T>;
end;

procedure TORMBrManagerDataSet.Close<T>;
begin
  FManagerDataSet.Close<T>;
end;

constructor TORMBrManagerDataSet.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  OwnerNestedList := True;
end;

function TORMBrManagerDataSet.Current<T>: T;
begin
  Result := FManagerDataSet.Current<T>;
end;

function TORMBrManagerDataSet.DataSet<T>: TDataSet;
begin
  Result := FManagerDataSet.DataSet<T>;
end;

destructor TORMBrManagerDataSet.Destroy;
begin
  if Assigned(FManagerDataSet) then
    FManagerDataSet.Free;
  inherited;
end;

procedure TORMBrManagerDataSet.EmptyDataSet<T>;
begin
  FManagerDataSet.EmptyDataSet<T>;
end;

function TORMBrManagerDataSet.Find<T>(const AID: Variant): T;
begin
  Result := FManagerDataSet.Find<T>(AID);
end;

function TORMBrManagerDataSet.Find<T>: TObjectList<T>;
begin
  Result := FManagerDataSet.Find<T>;
end;

function TORMBrManagerDataSet.FindWhere<T>(const AWhere, AOrderBy: string): TObjectList<T>;
begin
  Result := FManagerDataSet.FindWhere<T>(AWhere, AOrderBy);
end;

function TORMBrManagerDataSet.GetConnection: TDBEBrConnectionBase;
begin
  Result := FConnection;
end;

function TORMBrManagerDataSet.GetOwnerNestedList: Boolean;
begin
  Result := FManagerDataSet.OwnerNestedList;
end;

procedure TORMBrManagerDataSet.LoadLazy<T>(const AOwner: T);
begin
  FManagerDataSet.LoadLazy<T>(AOwner);
end;

function TORMBrManagerDataSet.NestedList<T>: TObjectList<T>;
begin
  Result := FManagerDataSet.NestedList<T>;
end;

procedure TORMBrManagerDataSet.Open<T>;
begin
  FManagerDataSet.Open<T>;
end;

procedure TORMBrManagerDataSet.Open<T>(const AID: Integer);
begin
  FManagerDataSet.Open<T>(AID);
end;

procedure TORMBrManagerDataSet.Open<T>(const AID: String);
begin
  FManagerDataSet.Open<T>(AID);
end;

procedure TORMBrManagerDataSet.OpenWhere<T>(const AWhere, AOrderBy: string);
begin
  FManagerDataSet.OpenWhere<T>(AWhere, AOrderBy);
end;

procedure TORMBrManagerDataSet.RefreshRecord<T>;
begin
  FManagerDataSet.RefreshRecord<T>;
end;

procedure TORMBrManagerDataSet.RemoveAdapter<T>;
begin
  FManagerDataSet.RemoveAdapter<T>;
end;

procedure TORMBrManagerDataSet.Save<T>(AObject: T);
begin
  FManagerDataSet.Save<T>(AObject);
end;

procedure TORMBrManagerDataSet.SetConnection(const Value: TDBEBrConnectionBase);
begin
  FConnection := Value;
  if Assigned(FManagerDataSet) then
    FManagerDataSet.Free;
  FManagerDataSet := TManagerDataSet.Create(FConnection.Connection);
end;

procedure TORMBrManagerDataSet.SetOwnerNestedList(const Value: Boolean);
begin
  FManagerDataSet.OwnerNestedList := Value;
end;

end.

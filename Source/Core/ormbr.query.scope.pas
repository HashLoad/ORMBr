unit ormbr.query.scope;

interface

uses
  SysUtils,
  Generics.Collections;

type
  TFuncList = TDictionary<String, TFunc<String>>;

  IQueryScope = interface
    ['{57DDFB1C-B262-4B8C-BC54-7B9827ACAE38}']
    procedure AddWhere(const AResource, AScopeName: String; const AFunc: TFunc<String>);
    procedure AddOrderBy(const AResource, AScopeName: String; const AFunc: TFunc<String>);
    function GetWhere(const AResource: String): TFuncList;
    function GetOrderBy(const AResource: String): TFuncList;
  end;

  TQueryScope = class(TInterfacedObject, IQueryScope)
  private
    class var FInstance: IQueryScope;
  private
    FScopeWhereList: TDictionary<String, TFuncList>;
    FScopeOrderByList: TDictionary<String, TFuncList>;
    constructor CreatePrivate;
  protected
    constructor Create;
  public
    destructor Destroy; override;
    class function GetInstance: IQueryScope;
    procedure AddWhere(const AResource, AScopeName: String; const AFunc: TFunc<String>);
    procedure AddOrderBy(const AResource, AScopeName: String; const AFunc: TFunc<String>);
    function GetWhere(const AResource: String): TFuncList;
    function GetOrderBy(const AResource: String): TFuncList;
  end;

implementation

{ TQueryScope }

procedure TQueryScope.AddOrderBy(const AResource, AScopeName: String;
  const AFunc: TFunc<String>);
var
  LResource: String;
  LScopeName: String;
begin
  LResource := UpperCase(AResource);
  LScopeName := UpperCase(AScopeName);
  if not FScopeOrderByList.ContainsKey(LResource) then
  begin
    FScopeOrderByList.Add(LResource, TFuncList.Create);
    if not FScopeOrderByList[LResource].ContainsKey(LScopeName) then
      FScopeOrderByList[LResource].Add(LScopeName, AFunc);
  end;
end;

procedure TQueryScope.AddWhere(const AResource, AScopeName: String;
  const AFunc: TFunc<String>);
var
  LResource: String;
  LScopeName: String;
begin
  LResource := UpperCase(AResource);
  LScopeName := UpperCase(AScopeName);
  if not FScopeWhereList.ContainsKey(LResource) then
  begin
    FScopeWhereList.Add(LResource, TFuncList.Create);
    if not FScopeWhereList[LResource].ContainsKey(LScopeName) then
      FScopeWhereList[LResource].Add(LScopeName, AFunc);
  end;
end;

constructor TQueryScope.Create;
begin
  raise Exception.Create('Para usar o IScopeQuery use o método TScopeQuery.GetInstance()');
end;

constructor TQueryScope.CreatePrivate;
begin
  FScopeWhereList := TObjectDictionary<String, TFuncList>.Create([doOwnsValues]);
  FScopeOrderByList := TObjectDictionary<String, TFuncList>.Create([doOwnsValues]);
end;

destructor TQueryScope.Destroy;
begin
  FScopeWhereList.Free;
  FScopeOrderByList.Free;
  inherited;
end;

class function TQueryScope.GetInstance: IQueryScope;
begin
  if not Assigned(FInstance) then
    FInstance := TQueryScope.CreatePrivate;
   Result := FInstance;
end;

function TQueryScope.GetOrderBy(const AResource: String): TFuncList;
begin
  Result := nil;
  if not FScopeOrderByList.ContainsKey(AResource) then
    Exit;
  Result := FScopeOrderByList[AResource];
end;

function TQueryScope.GetWhere(const AResource: String): TFuncList;
begin
  Result := nil;
  if not FScopeWhereList.ContainsKey(AResource) then
    Exit;
  Result := FScopeWhereList[AResource];
end;

end.

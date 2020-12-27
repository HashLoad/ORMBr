unit ormbr.scope;

interface

uses
  SysUtils,
  Generics.Collections;

type
  TFuncList = TDictionary<String, TFunc<String>>;

  TORMBrScope = class
  private
    class var FScopeWhereList: TDictionary<String, TFuncList>;
    class var FScopeOrderByList: TDictionary<String, TFuncList>;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure AddWhere(const AResource, AScopeName: String; const AFunc: TFunc<String>);
    class procedure AddOrderBy(const AResource, AScopeName: String; const AFunc: TFunc<String>);
    class function GetWhere(const AResource: String): TFuncList;
    class function GetOrderBy(const AResource: String): TFuncList;
  end;

implementation

{ TORMBrScope }

class procedure TORMBrScope.AddOrderBy(const AResource, AScopeName: String;
  const AFunc: TFunc<String>);
begin
  if not FScopeOrderByList.ContainsKey(AResource) then
  begin
    FScopeOrderByList.Add(AResource, TFuncList.Create);
    if not FScopeOrderByList[AResource].ContainsKey(AScopeName) then
      FScopeOrderByList[AResource].Add(AScopeName, AFunc);
  end;
end;

class procedure TORMBrScope.AddWhere(const AResource, AScopeName: String;
  const AFunc: TFunc<String>);
begin
  if not FScopeWhereList.ContainsKey(AResource) then
  begin
    FScopeWhereList.Add(AResource, TFuncList.Create);
    if not FScopeWhereList[AResource].ContainsKey(AScopeName) then
      FScopeWhereList[AResource].Add(AScopeName, AFunc);
  end;
end;

class constructor TORMBrScope.Create;
begin
  FScopeWhereList := TObjectDictionary<String, TFuncList>.Create([doOwnsValues]);
  FScopeOrderByList := TObjectDictionary<String, TFuncList>.Create([doOwnsValues]);
end;

class destructor TORMBrScope.Destroy;
begin
  FScopeWhereList.Free;
  FScopeOrderByList.Free;
end;

class function TORMBrScope.GetOrderBy(const AResource: String): TFuncList;
begin
  Result := nil;
  if not FScopeOrderByList.ContainsKey(AResource) then
    Exit;
  Result := FScopeOrderByList[AResource];
end;

class function TORMBrScope.GetWhere(const AResource: String): TFuncList;
begin
  Result := nil;
  if not FScopeWhereList.ContainsKey(AResource) then
    Exit;
  Result := FScopeWhereList[AResource];
end;

end.

unit ormbr.criteria.resultset;

interface

uses
  Generics.Collections,
//  ormbr.criteria,
  dbebr.factory.interfaces,
  ormbr.bind;

type
  ICriteriaSet = interface
    ['{0285A016-824C-41C7-9680-44127BB62AD0}']
    function SetConnection(const AConnection: IDBConnection): ICriteriaSet;
//    function SQL(const ACriteria: ICriteria): ICriteriaSet; overload;
    function SQL(const ASQL: string): ICriteriaSet; overload;
    function AsResultSet: IDBResultSet;
  end;

  TCriteria = class(TInterfacedObject, ICriteriaSet)
  private
    FSQL: string;
    FConnection: IDBConnection;
  public
    class function New: ICriteriaSet;
    function SetConnection(const AConnection: IDBConnection): ICriteriaSet;
//    function SQL(const ACriteria: ICriteria): ICriteriaSet; overload;
    function SQL(const ASQL: string): ICriteriaSet; overload;
    function AsResultSet: IDBResultSet;
  end;

  ICriteriaObject<M: class, constructor> = interface
    ['{E1AA571D-E8BC-4A79-8B67-D7E77680F29C}']
    function SetConnection(AConnection: IDBConnection): ICriteriaObject<M>;
//    function SQL(ACriteria: ICriteria): ICriteriaObject<M>; overload;
    function SQL(ASQL: string): ICriteriaObject<M>; overload;
    function AsList: TObjectList<M>;
    function AsValue: M;
  end;

  TCriteria<M: class, constructor> = class(TInterfacedObject, ICriteriaObject<M>)
  private
    FSQL: string;
    FConnection: IDBConnection;
  public
    class function New: ICriteriaObject<M>;
    function SetConnection(AConnection: IDBConnection): ICriteriaObject<M>;
//    function SQL(ACriteria: ICriteria): ICriteriaObject<M>; overload;
    function SQL(ASQL: string): ICriteriaObject<M>; overload;
    function AsList: TObjectList<M>;
    function AsValue: M;
  end;

implementation

{ TCriteria<M> }

function TCriteria.AsResultSet: IDBResultSet;
begin
  Result := FConnection.CreateResultSet(FSQL);
end;

class function TCriteria.New: ICriteriaSet;
begin
  Result := Self.Create;
end;

function TCriteria.SetConnection(const AConnection: IDBConnection): ICriteriaSet;
begin
  FConnection := AConnection;
  Result := Self;
end;

function TCriteria.SQL(const ASQL: string): ICriteriaSet;
begin
  FSQL := ASQL;
  Result := Self;
end;

//function TCriteria.SQL(const ACriteria: ICriteria): ICriteriaSet;
//begin
//  FSQL := ACriteria.AsString;
//  Result := Self;
//end;

function TCriteria<M>.AsList: TObjectList<M>;
var
  LResultSet: IDBResultSet;
  LObject: M;
begin
  LResultSet := FConnection.CreateResultSet(FSQL);
  try
    if LResultSet.RecordCount = 0 then
      Exit(nil);
    Result := TObjectList<M>.Create;
    while LResultSet.NotEof do
    begin
      LObject := M.Create;
      TBind.Instance.SetFieldToProperty(LResultSet, LObject);
      Result.Add(LObject);
    end;
  finally
    LResultSet.Close;
    FConnection.Disconnect;
  end;
end;

function TCriteria<M>.AsValue: M;
var
  LResultSet: IDBResultSet;
begin
  LResultSet := FConnection.CreateResultSet(FSQL);
  try
    if LResultSet.RecordCount = 0 then
      Exit(nil);
    Result := M.Create;
    TBind.Instance.SetFieldToProperty(LResultSet, Result);
  finally
    LResultSet.Close;
    FConnection.Disconnect;
  end;
end;

class function TCriteria<M>.New: ICriteriaObject<M>;
begin
  Result := Self.Create;
end;

//function TCriteria<M>.SQL(ACriteria: ICriteria): ICriteriaObject<M>;
//begin
//  FSQL := ACriteria.AsString;
//  Result := Self;
//end;

function TCriteria<M>.SetConnection(AConnection: IDBConnection): ICriteriaObject<M>;
begin
  FConnection := AConnection;
  Result := Self;
end;

function TCriteria<M>.SQL(ASQL: string): ICriteriaObject<M>;
begin
  FSQL := ASQL;
  Result := Self;
end;

end.

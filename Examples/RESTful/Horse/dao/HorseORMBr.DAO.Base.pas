unit HorseORMBr.DAO.Base;

interface

uses
  FireDAC.Comp.Client,
  dbebr.factory.interfaces,
  dbebr.factory.firedac,
  ormbr.container.objectset.interfaces,
  ormbr.container.objectset,
  System.Generics.Collections;

type
  THorseORMBrDAOBase<T: class, constructor> = class
  protected
    FConnection : IDBConnection;
    FORMBrContainer : IContainerObjectSet<T>;
  public
    procedure insert(Value: T);
    procedure update(Value: T);
    procedure delete(Value: T);
    procedure modify(Value: T);

    function listAll: TObjectList<T>;
    function findWhere(AWhere: String): T;

    constructor create(Connection: TFDConnection);
end;

implementation

{ THorseORMBrDAOBase<T> }

constructor THorseORMBrDAOBase<T>.create(Connection: TFDConnection);
begin
  FConnection := TFactoryFiredac.Create(Connection, dnFirebird);
  FORMBrContainer := TContainerObjectSet<T>.Create(FConnection);
end;

procedure THorseORMBrDAOBase<T>.delete(Value: T);
begin
  FORMBrContainer.Delete(Value);
end;

function THorseORMBrDAOBase<T>.findWhere(AWhere: String): T;
var
  list : TObjectList<T>;
  i: Integer;
begin
  result := nil;
  list := FORMBrContainer.FindWhere(AWhere);
  try
    list.OwnsObjects := False;
    if list.Count > 0 then
      result := list.First;

    for i := 1 to list.Count - 1 do
      list[i].free;
  finally
    list.Free;
  end;
end;

procedure THorseORMBrDAOBase<T>.insert(Value: T);
begin
  FORMBrContainer.Insert(Value);
end;

function THorseORMBrDAOBase<T>.listAll: TObjectList<T>;
begin
  result := FORMBrContainer.Find;
end;

procedure THorseORMBrDAOBase<T>.modify(Value: T);
begin
  FORMBrContainer.Modify(Value);
end;

procedure THorseORMBrDAOBase<T>.update(Value: T);
begin
  FORMBrContainer.Update(Value);
end;

end.

unit ormbr.dml.cache;

interface

uses
  Generics.Collections;

type
  TDMLCache = class
  private
    class var
    FDMLCache: TDictionary<String, String>;
  public
    class constructor Create;
    class destructor Destroy;
    class function DMLCache: TDictionary<String, String>;
  end;

implementation

{ TDMLCache }

class constructor TDMLCache.Create;
begin
  FDMLCache := TDictionary<String, String>.Create;
end;

class destructor TDMLCache.Destroy;
begin
  FDMLCache.Free;
end;

class function TDMLCache.DMLCache: TDictionary<String, String>;
begin
  Result := FDMLCache;
end;

end.

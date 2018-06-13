unit Helper.Dataset;

interface
  uses
    DB;

  type
    TDatasetHelper = class helper for TDataSet
      function GetNextKeyValue(const AKeyFieldName: string): Integer;
    end;

implementation

function TDatasetHelper.GetNextKeyValue(const AKeyFieldName: string): Integer;
begin
  Self.First;
  Result := 0;
  while not Self.Eof do
  begin
    if Result < FieldByName(AKeyFieldName).AsInteger then
      Result := FieldByName(AKeyFieldName).AsInteger;
    Next;
  end;
end;

end.

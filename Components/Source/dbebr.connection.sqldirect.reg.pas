unit dbebr.connection.sqldirect.reg;

interface

uses
  Classes,
  DesignIntf,
  DesignEditors,
  dbebr.connection.sqldirect;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('DBEBr', [TDBEBrConnectionSQLDirect]);
end;

end.

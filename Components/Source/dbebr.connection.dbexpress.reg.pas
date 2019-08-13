unit dbebr.connection.dbexpress.reg;

interface

uses
  Classes,
  DesignIntf,
  DesignEditors,
  dbebr.connection.dbexpress;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('DBEBr', [TDBEBrConnectionDBExpress]);
end;

end.

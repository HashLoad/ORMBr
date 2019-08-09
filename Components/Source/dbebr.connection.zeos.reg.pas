unit dbebr.connection.zeos.reg;

interface

uses
  Classes,
  DesignIntf,
  DesignEditors,
  dbebr.connection.zeos;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('DBEBr', [TDBEBrConnectionZeos]);
end;

end.

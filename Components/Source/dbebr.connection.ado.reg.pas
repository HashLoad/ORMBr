unit dbebr.connection.ado.reg;

interface

uses
  Classes,
  DesignIntf,
  DesignEditors,
  dbebr.connection.ado;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('DBEBr', [TDBEBrConnectionADO]);
end;

end.

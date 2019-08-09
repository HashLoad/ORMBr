unit dbebr.connection.firedac.reg;

interface

uses
  Classes,
  DesignIntf,
  DesignEditors,
  dbebr.connection.firedac;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('DBEBr', [TDBEBrConnectionFireDAC]);
end;

end.

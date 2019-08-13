unit dbebr.connection.nexusdb.reg;

interface

uses
  Classes,
  DesignIntf,
  DesignEditors,
  dbebr.connection.nexusdb;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('DBEBr', [TDBEBrConnectionNexusDB]);
end;

end.

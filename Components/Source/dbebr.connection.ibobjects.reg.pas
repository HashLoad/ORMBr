unit dbebr.connection.ibobjects.reg;

interface

uses
  Classes,
  DesignIntf,
  DesignEditors,
  dbebr.connection.ibobjects;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('DBEBr', [TDBEBrConnectionIBObjects]);
end;

end.

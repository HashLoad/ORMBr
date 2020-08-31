unit dbebr.connection.mongowire.reg;

interface

uses
  Classes,
  DesignIntf,
  DesignEditors,
  dbebr.connection.mongowire;

procedure register;

implementation

{$R 'dbebr.connection.mongowire.res' 'dbebr.connection.mongowire.rc'}

procedure register;
begin
  RegisterComponents('DBEBr-NoSQL', [TDBEBrConnectionMongoWire]);
end;

end.

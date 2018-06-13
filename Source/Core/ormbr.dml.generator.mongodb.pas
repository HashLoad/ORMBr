unit ormbr.dml.generator.mongodb;

interface

uses
  DB,
  Classes,
  Generics.Collections,
  ormbr.dml.generator.nosql,
  ormbr.mapping.classes,
  ormbr.factory.interfaces,
  ormbr.driver.register,
  ormbr.dml.commands;

type
  /// <summary>
  /// Classe de conexão concreta com NoSQL
  /// </summary>
  TDMLGeneratorMongoDB = class(TDMLGeneratorNoSQL)
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TDMLGeneratorMongoDB }

constructor TDMLGeneratorMongoDB.Create;
begin
  inherited;
  FDateFormat := 'yyyy-mm-dd';
  FTimeFormat := 'HH:MM:SS';
end;

destructor TDMLGeneratorMongoDB.Destroy;
begin

  inherited;
end;

initialization
  TDriverRegister.RegisterDriver(dnMongoDB, TDMLGeneratorMongoDB.Create);

end.

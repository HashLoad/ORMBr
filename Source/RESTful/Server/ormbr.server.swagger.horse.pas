unit ormbr.server.swagger.horse;

interface

uses
  SysUtils,
  GBSwagger.Model.Interfaces,
  GBSwagger.Model.Attributes,
  ormbr.server.resource.horse,
  ormbr.model.master;

type
  TORMBrSwagger = class
  strict private
    class var FResource: String;
  public
    class constructor Create;
    class property Resource: String read FResource write FResource;
  end;

implementation

{ TORMBrSwagger }

class constructor TORMBrSwagger.Create;
begin
  FResource := 'Resourcename';
end;

initialization

  Swagger
//    .Register
//      .SchemaOnError(Exception)
//    .&End
    .BasePath('ormbr/swagger/doc')
    .Info
      .Title('ORMBr Server')
      .Description('API RESTful')
      .Contact
        .Name('Contact Isaque Pinheiro')
        .Email('isaquesp@gmail.com.br')
        .URL('https://www.isaquepinheiro.com.br')
      .&End
    .&End
    .BasePath('v1');

    Swagger.Path(Format('api/ormbr/:%s', [TORMBrSwagger.Resource]))
      .Tag(Format('%s', [TORMBrSwagger.Resource]))
      .GET.Summary('Select')
      .AddResponse(400).&End
      .AddResponse(200, 'Json data').Schema(Tmaster).&End;

    Swagger.Path(Format('api/ormbr/:%s(id)', [TORMBrSwagger.Resource]))
      .Tag(Format('%s', [TORMBrSwagger.Resource]))
      .GET.Summary('Select')
      .AddParamPath('id', Format('%s ID', [TORMBrSwagger.Resource])).&End
      .AddResponse(400).&End
      .AddResponse(200, 'Json data').Schema(Tmaster).&End;

    Swagger.Path(Format('api/ormbr/:%s?$', [TORMBrSwagger.Resource]))
      .Tag(Format('%s', [TORMBrSwagger.Resource]))
      .GET.Summary('Select')
      .AddParamQuery('query', 'OData (Open Data Protocol)').&End
      .Description('https://www.odata.org/getting-started/basic-tutorial/')
      .AddResponse(400).&End
      .AddResponse(200, 'Json data').Schema(Tmaster).&End;

// Exemples (OData):
// http://localhost:9000/api/ormbr/master
// http://localhost:9000/api/ormbr/master(7)
// http://localhost:9000/api/ormbr/master?$filter=master_id eq 7
// http://localhost:9000/api/ormbr/master?$filter=master_id gt 1 and master_id lt 10&$orderby=description desc
// http://localhost:9000/api/ormbr/master?$skip=2&$top=1

finalization

end.

program pViaCEP;

uses
  Vcl.Forms,
  uPrincipal in 'uPrincipal.pas' {Form2},
  ormbr.viacep in 'ormbr.viacep.pas',
  ormbr.client.ws in '..\..\..\Source\RESTful Components\Client\ormbr.client.ws.pas',
  ormbr.driver.rest.ws in '..\..\..\Source\RESTful Components\Client\ormbr.driver.rest.ws.pas',
  ormbr.factory.rest.ws in '..\..\..\Source\RESTful Components\Client\ormbr.factory.rest.ws.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

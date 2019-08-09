program PJSON;

uses
  Forms,
  uJSON in 'uJSON.pas' {Form4},
  ormbr.model.person in 'ormbr.model.person.pas',
  ormbr.types.nullable in '..\..\Source\Core\ormbr.types.nullable.pas',
  ormbr.json in '..\..\Source\Core\ormbr.json.pas',
  ormbr.mapping.rttiutils in '..\..\Source\Core\ormbr.mapping.rttiutils.pas',
  ormbr.rtti.helper in '..\..\Source\Core\ormbr.rtti.helper.pas',
  ormbr.rest.json in '..\..\Source\Core\ormbr.rest.json.pas',
  ormbr.json.utils in '..\..\Source\Core\ormbr.json.utils.pas',
  ormbr.encddecd in '..\..\Source\Core\ormbr.encddecd.pas',
  ormbr.types.blob in '..\..\Source\Core\ormbr.types.blob.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.

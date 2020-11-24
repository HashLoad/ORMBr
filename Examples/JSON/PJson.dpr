program PJSON;

uses
  Forms,
  uJSON in 'uJSON.pas' {Form4},
  ormbr.model.person in 'ormbr.model.person.pas',
  ormbr.types.nullable in '..\..\Source\Core\ormbr.types.nullable.pas',
  ormbr.rtti.helper in '..\..\Source\Core\ormbr.rtti.helper.pas',
  ormbr.types.blob in '..\..\Source\Core\ormbr.types.blob.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.

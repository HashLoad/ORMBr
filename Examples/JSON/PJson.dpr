program PJSON;

uses
  Forms,
  uJSON in 'uJSON.pas' {Form4},
  ormbr.model.person in 'ormbr.model.person.pas',
  jsonbr.utils in '..\..\..\JSONBr\Source\Core\jsonbr.utils.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.

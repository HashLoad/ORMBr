program PJSON;

uses
  Forms,
  uJSON in 'uJSON.pas' {Form4},
  ormbr.model.person in 'ormbr.model.person.pas',
  jsonbr.utils in '..\..\..\JSONBr\Source\Core\jsonbr.utils.pas',
  jsonbr.builders in '..\..\..\JSONBr\Source\Core\jsonbr.builders.pas',
  jsonbr.reader in '..\..\..\JSONBr\Source\Reader\jsonbr.reader.pas',
  jsonbr.writer in '..\..\..\JSONBr\Source\Writer\jsonbr.writer.pas',
  jsonbr.types in '..\..\..\JSONBr\Source\Core\jsonbr.types.pas',
  jsonbr in '..\..\..\JSONBr\Source\jsonbr.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.

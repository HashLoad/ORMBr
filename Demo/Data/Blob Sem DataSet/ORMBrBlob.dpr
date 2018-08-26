program ORMBrBlob;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {Form2},
  ormbr.model.PERSON in 'ormbr.model.PERSON.pas',
  ormbr.encddecd in '..\..\..\Source\Core\ormbr.encddecd.pas',
  ormbr.json in '..\..\..\Source\Core\ormbr.json.pas',
  ormbr.json.utils in '..\..\..\Source\Core\ormbr.json.utils.pas',
  ormbr.rest.json in '..\..\..\Source\Core\ormbr.rest.json.pas',
  ormbr.types.blob in '..\..\..\Source\Core\ormbr.types.blob.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

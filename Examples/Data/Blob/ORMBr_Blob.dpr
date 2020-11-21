program ORMBr_Blob;

uses
  Vcl.Forms,
  UPrincipal in 'UPrincipal.pas' {Form2},
  ormbr.model.PERSON in 'ormbr.model.PERSON.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

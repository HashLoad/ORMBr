program ORMBr_VariosNiveisDeDados;

uses
  Vcl.Forms,
  Principal in 'Principal.pas' {Form1},
  orion.model.cidade in 'orion.model.cidade.pas',
  orion.model.contato in 'orion.model.contato.pas',
  orion.model.emailcontato in 'orion.model.emailcontato.pas',
  orion.model.empresa in 'orion.model.empresa.pas',
  orion.model.estado in 'orion.model.estado.pas',
  orion.model.redesocialcontato in 'orion.model.redesocialcontato.pas',
  orion.model.telefonecontato in 'orion.model.telefonecontato.pas',
  orion.model.usuario in 'orion.model.usuario.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

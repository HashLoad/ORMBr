program ORMBr_QuatroNiveisDeDados;

uses
  Vcl.Forms,
  Principal in 'Principal.pas' {Form1},
  Model.Atendimento in 'Model.Atendimento.pas',
  Model.Exame in 'Model.Exame.pas',
  Model.Procedimento in 'Model.Procedimento.pas',
  Model.Setor in 'Model.Setor.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

program Exemplo_ORMBr;

uses
  Vcl.Forms,
  Principal in 'Principal.pas' {Form1},
  Model.Atendimento in 'Model.Atendimento.pas',
  Model.Exame in 'Model.Exame.pas',
  Model.Procedimento in 'Model.Procedimento.pas',
  UDM_Conexao in 'UDM_Conexao.pas' {DataModule1: TDataModule},
  Model.Setor in 'Model.Setor.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

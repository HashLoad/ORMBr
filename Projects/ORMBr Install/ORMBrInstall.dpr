program ORMBrInstall;

uses
  Forms,
  uPrincipal in 'uPrincipal.pas' {frmPrincipal},
  uFrameLista in 'uFrameLista.pas' {framePacotes: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Instala��o ORMBr - REST Server/Client Componentes';
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.Run;
end.

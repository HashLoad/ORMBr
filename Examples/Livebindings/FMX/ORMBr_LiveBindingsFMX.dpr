program ORMBr_LiveBindingsFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  UPrincipal in 'UPrincipal.pas' {FormPrincipal},
  produto in 'produto.pas',
  ormbr.controls.helpers in '..\..\Source\ormbr.controls.helpers.pas',
  ormbr.fmx.controls in '..\..\Source\ormbr.fmx.controls.pas',
  ormbr.livebindings in '..\..\Source\ormbr.livebindings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPrincipal, FormPrincipal);
  Application.Run;
end.

program ORMBr_LiveBindingsFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  UPrincipal in 'UPrincipal.pas' {FormPrincipal},
  produto in 'produto.pas',
  ormbr.controls.helpers in '..\..\..\Source\Livebindings\ormbr.controls.helpers.pas',
  ormbr.fmx.controls in '..\..\..\Source\Livebindings\ormbr.fmx.controls.pas',
  ormbr.livebindings in '..\..\..\Source\Livebindings\ormbr.livebindings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPrincipal, FormPrincipal);
  Application.Run;
end.

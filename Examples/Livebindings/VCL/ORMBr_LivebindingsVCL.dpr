program ORMBr_LivebindingsVCL;

uses
  Vcl.Forms,
  UPrincipal in 'UPrincipal.pas' {FormPrincipal},
  produto in 'produto.pas',
  ormbr.controls.helpers in '..\..\..\Source\Livebindings\ormbr.controls.helpers.pas',
  ormbr.livebindings in '..\..\..\Source\Livebindings\ormbr.livebindings.pas',
  ormbr.vcl.controls in '..\..\..\Source\Livebindings\ormbr.vcl.controls.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormPrincipal, FormPrincipal);
  Application.Run;
end.

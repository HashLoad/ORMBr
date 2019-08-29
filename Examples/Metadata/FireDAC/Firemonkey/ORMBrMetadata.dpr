program ORMBrMetadata;

uses
  System.StartUpCopy,
  FMX.Forms,
  uPrincipal in 'uPrincipal.pas' {Form4},
  ormbr.model.client in '..\..\..\Data\Models\ormbr.model.client.pas',
  ormbr.model.detail in '..\..\..\Data\Models\ormbr.model.detail.pas',
  ormbr.model.lookup in '..\..\..\Data\Models\ormbr.model.lookup.pas',
  ormbr.model.master in '..\..\..\Data\Models\ormbr.model.master.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.

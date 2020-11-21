program ORMBr_Firedac;

uses
  Forms,
  SysUtils,
  uMainFormORM in 'uMainFormORM.pas' {Form3},
  ormbr.model.client in 'ormbr.model.client.pas',
  ormbr.model.detail in 'ormbr.model.detail.pas',
  ormbr.model.lookup in 'ormbr.model.lookup.pas',
  ormbr.model.master in 'ormbr.model.master.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.

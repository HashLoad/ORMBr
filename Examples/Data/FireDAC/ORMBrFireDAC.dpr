program ORMBrFireDAC;

uses
  Forms,
  SysUtils,
  uMainFormORM in 'uMainFormORM.pas' {Form3},
  ormbr.model.client in 'ormbr.model.client.pas',
  ormbr.model.detail in 'ormbr.model.detail.pas',
  ormbr.model.lookup in 'ormbr.model.lookup.pas',
  ormbr.model.master in 'ormbr.model.master.pas',
  ormbr.driver.firedac in '..\..\..\Source\Drivers\ormbr.driver.firedac.pas',
  ormbr.driver.firedac.transaction in '..\..\..\Source\Drivers\ormbr.driver.firedac.transaction.pas',
  ormbr.factory.firedac in '..\..\..\Source\Drivers\ormbr.factory.firedac.pas',
  ormbr.rtti.helper in '..\..\..\Source\Core\ormbr.rtti.helper.pas',
  ormbr.factory.connection in '..\..\..\Source\Drivers\ormbr.factory.connection.pas',
  ormbr.command.selecter in '..\..\..\Source\Core\ormbr.command.selecter.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.

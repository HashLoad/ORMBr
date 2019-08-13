program ORMBrFireDAC;

uses
  Forms,
  SysUtils,
  uMainFormORM in 'uMainFormORM.pas' {Form3},
  ormbr.restdataset.fdmemtable in '..\..\..\Source\RESTFul\ormbr.restdataset.fdmemtable.pas',
  ormbr.restdataset.adapter in '..\..\..\Source\RESTFul\ormbr.restdataset.adapter.pas',
  ormbr.session.datasnap in '..\..\..\Source\RESTFul\ormbr.session.datasnap.pas',
  ormbr.model.client in '..\..\Data\Models\ormbr.model.client.pas',
  ormbr.model.detail in '..\..\Data\Models\ormbr.model.detail.pas',
  ormbr.model.lookup in '..\..\Data\Models\ormbr.model.lookup.pas',
  ormbr.model.master in '..\..\Data\Models\ormbr.model.master.pas',
  ormbr.session.baseurl in '..\..\..\Source\RESTFul\ormbr.session.baseurl.pas',
  ormbr.container.restfdmemtable in '..\..\..\Source\RESTFul\ormbr.container.restfdmemtable.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.

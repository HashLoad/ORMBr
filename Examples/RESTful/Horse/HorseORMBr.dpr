program HorseORMBr;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Horse,
  System.SysUtils,
  DM.Connection in 'providers\DM.Connection.pas' {DMConn: TDataModule},
  ormbr.model.client in 'models\ormbr.model.client.pas',
  ormbr.model.detail in 'models\ormbr.model.detail.pas',
  ormbr.model.lookup in 'models\ormbr.model.lookup.pas',
  ormbr.model.master in 'models\ormbr.model.master.pas',
  HorseORMBr.DAO.Base in 'dao\HorseORMBr.DAO.Base.pas',
  HorseORMBr.Controller.Master in 'controller\HorseORMBr.Controller.Master.pas',
  HorseORMBr.Controller.Client in 'controller\HorseORMBr.Controller.Client.pas',
  System.Classes;

begin
  ReportMemoryLeaksOnShutdown := True;
  IsConsole := False;

  THorse.Get('master', HorseORMBr.Controller.Master.List);
  THorse.Post('master', HorseORMBr.Controller.Master.Insert);
  THorse.Get('master/:id', HorseORMBr.Controller.Master.Find);
  THorse.Put('master/:id', HorseORMBr.Controller.Master.Update);
  THorse.Delete('master/:id', HorseORMBr.Controller.Master.Delete);

  THorse.Get('client', HorseORMBr.Controller.Client.List);
  THorse.Post('client', HorseORMBr.Controller.Client.Insert);
  THorse.Get('client/:id', HorseORMBr.Controller.Client.Find);
  THorse.Put('client/:id', HorseORMBr.Controller.Client.Update);
  THorse.Delete('client/:id', HorseORMBr.Controller.Client.Delete);

  THorse.Listen(9000, '127.0.0.1',
    procedure
    begin
      Readln;
    end
  );

end.

(*
  Copyright 2016, MARS-Curiosity library

  Home: https://github.com/andrea-magni/MARS
*)
unit Server.Forms.Main;

{$I MARS.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  ActnList,
  System.Actions,

  Diagnostics,

  MARS.Core.Engine,
  MARS.http.Server.Indy,
  {$IFDEF MSWINDOWS}
  MARS.mORMotJWT.Token,
  {$ELSE}
  MARS.JOSEJWT.Token,
  {$ENDIF}
  MARS.Core.Application,

  ormbr.factory.interfaces,
  ormbr.factory.firedac,
  ormbr.server.mars;

type
  TMainForm = class(TForm)
    TopPanel: TPanel;
    StartButton: TButton;
    StopButton: TButton;
    MainActionList: TActionList;
    StartServerAction: TAction;
    StopServerAction: TAction;
    PortNumberEdit: TEdit;
    Label1: TLabel;
    procedure StartServerActionExecute(Sender: TObject);
    procedure StartServerActionUpdate(Sender: TObject);
    procedure StopServerActionExecute(Sender: TObject);
    procedure StopServerActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    FServer: TMARShttpServerIndy;
    FEngine: TMARSEngine;
    RESTServerMARS: TRESTServerMARS;
    FConnection: IDBConnection;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  MARS.Core.MessageBodyWriter,
  MARS.Core.MessageBodyWriters,
  MARS.Core.URL,
  MARS.Utils.Parameters.IniFile,
  Server.Data.Main;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopServerAction.Execute;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // MARS-Curiosity Engine
  FEngine := TMARSEngine.Create;
  try
    FEngine.Parameters.LoadFromIniFile;
    FEngine.AddApplication('DefaultApp', '/default', ['Server.Resources.*']);

    /// ORMBr - REST Server MARS
    FConnection := TFactoryFireDAC.Create(ServerDataModule.FDConnection1, dnSQLite);
    RESTServerMARS := TRESTServerMARS.Create(Self);
    RESTServerMARS.MARSEngine := FEngine;
    RESTServerMARS.Connection := FConnection;
  except
    FreeAndNil(FEngine);
    raise;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  RESTServerMARS.Free;
  FreeAndNil(FEngine);
end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  FEngine.Port := StrToInt(PortNumberEdit.Text);
  // http server implementation
  FServer := TMARShttpServerIndy.Create(FEngine);
  try
    FServer.DefaultPort := FEngine.Port;
    FServer.Active := True;
  except
    FServer.Free;
    raise;
  end;
end;

procedure TMainForm.StartServerActionUpdate(Sender: TObject);
begin
  StartServerAction.Enabled := (FServer = nil) or (FServer.Active = False);
end;

procedure TMainForm.StopServerActionExecute(Sender: TObject);
begin
  FServer.Active := False;
  FreeAndNil(FServer);
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := Assigned(FServer) and (FServer.Active = True);
end;

end.

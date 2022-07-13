{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2017 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Server.Forms.Main;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.ActnList,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.Diagnostics, System.Actions,

  /// WiRL
  WiRL.http.Server,
  WiRL.http.Server.Indy,
  WiRL.Core.Engine,
  WiRL.Core.Application,
  WiRL.Core.Registry,
  WiRL.Core.Attributes,
  WiRL.http.Accept.MediaType,
  WiRL.http.URL,
  WiRL.Core.MessageBody.Default,
  WiRL.Core.Auth.Context,
  WiRL.http.Request,
  WiRL.http.Response,

  ormbr.factory.interfaces,
  ormbr.factory.firedac,
  ormbr.server.wirl;

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
  private
    RESTServer: TWiRLServer;
    RESTServerWiRL1: TRESTServerWiRL;
    FConnection: IDBConnection;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  Server.Datamodule;

{$R *.dfm}

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopServerAction.Execute;
  RESTServerWiRL1.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  RESTServer := TWiRLServer.Create(Self);
  RESTServer.AddEngine<TWiRLEngine>('/rest')
    .SetEngineName('RESTEngine')
    .AddApplication('/app')
     .SetResources('*')
      .SetFilters('*');

  /// ORMBr - REST Server WiRL
  FConnection := TFactoryFireDAC.Create(ServerDataModule.FDConnection1, dnSQLite);

  RESTServerWiRL1 := TRESTServerWiRL.Create(Self);
  RESTServerWiRL1.WiRLEngine := RESTServer.GetEngine('/rest') as TWiRLEngine;
  RESTServerWiRL1.Connection := FConnection;
end;

procedure TMainForm.StartServerActionExecute(Sender: TObject);
begin
  RESTServer.Port := StrToIntDef(PortNumberEdit.Text, 211);
  if not RESTServer.Active then
    RESTServer.Active := True;
end;

procedure TMainForm.StartServerActionUpdate(Sender: TObject);
begin
  StartServerAction.Enabled := not Assigned(RESTServer) or not RESTServer.Active;
end;

procedure TMainForm.StopServerActionExecute(Sender: TObject);
begin
  RESTServer.Active := False;
end;

procedure TMainForm.StopServerActionUpdate(Sender: TObject);
begin
  StopServerAction.Enabled := Assigned(RESTServer) and RESTServer.Active;
end;

end.

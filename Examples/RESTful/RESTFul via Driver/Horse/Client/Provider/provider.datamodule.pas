unit provider.datamodule;

interface

uses
  DB,
  SysUtils,
  Classes,
  ormbr.client,
  ormbr.client.base,
  ormbr.client.methods,
  ormbr.client.horse,

  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, Datasnap.DBClient;

type
  TProviderDM = class(TDataModule)
    FDMaster: TFDMemTable;
    FDDetail: TFDMemTable;
    FDClient: TFDMemTable;
    FDLookup: TFDMemTable;
    procedure RESTClientHorse1AfterCommand(AStatusCode: Integer;
      var AResponseString: string; ARequestMethod: string);
    procedure RESTClientHorse1BeforeCommand(ARequestMethod: string);
    procedure RESTClientHorse1ErrorCommand(const AURLBase, AResource,
      ASubResource, ARequestMethod, AMessage: string;
      const AResponseCode: Integer);
    procedure RESTClientHorse1Authentication;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    RESTClientHorse1: TRESTClientHorse;
  end;

//var
//  ProviderDM: TProviderDM;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TProviderDM.DataModuleCreate(Sender: TObject);
begin
  RESTClientHorse1 := TRESTClientHorse.Create(nil);
  RESTClientHorse1.Host := 'localhost';
  RESTClientHorse1.Port := 9000;
  RESTClientHorse1.ORMBrServerUse := True;
  RESTClientHorse1.OnAuthentication := RESTClientHorse1Authentication;
  RESTClientHorse1.OnBeforeCommand := RESTClientHorse1BeforeCommand;
  RESTClientHorse1.OnAfterCommand := RESTClientHorse1AfterCommand;
//  RESTClientHorse1.OnErrorCommand := RESTClientHorse1ErrorCommand;
end;

procedure TProviderDM.DataModuleDestroy(Sender: TObject);
begin
  RESTClientHorse1.Free;
end;

procedure TProviderDM.RESTClientHorse1AfterCommand(AStatusCode: Integer;
  var AResponseString: string; ARequestMethod: string);
begin
  //
end;

procedure TProviderDM.RESTClientHorse1Authentication;
begin
  // Authentication Basic
  //  RESTClientHorse1.Authenticator.Token := Base64(RESTClientHorse1.Authenticator.Username + ':' + RESTClientHorse1.Authenticator.Password);

  // Authentication Bearer
  //  RESTClientHorse1.Authenticator.Token := GetToken();
end;

procedure TProviderDM.RESTClientHorse1BeforeCommand(ARequestMethod: string);
begin
  // Pega aqui os erros e criei seu proprio log.
end;

procedure TProviderDM.RESTClientHorse1ErrorCommand(const AURLBase, AResource,
  ASubResource, ARequestMethod, AMessage: string; const AResponseCode: Integer);
begin
  //
end;

end.

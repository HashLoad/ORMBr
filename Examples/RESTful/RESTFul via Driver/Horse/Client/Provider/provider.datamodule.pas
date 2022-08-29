unit provider.datamodule;

interface

uses
  DB,
  SysUtils,
  Classes,
  ormbr.rest.classes,
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
  FireDAC.Comp.Client;

type
  TProviderDM = class(TDataModule)
    RESTClientHorse1: TRESTClientHorse;
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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

//var
//  ProviderDM: TProviderDM;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

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
  //
end;

procedure TProviderDM.RESTClientHorse1ErrorCommand(const AURLBase, AResource,
  ASubResource, ARequestMethod, AMessage: string; const AResponseCode: Integer);
begin
  //
end;

end.

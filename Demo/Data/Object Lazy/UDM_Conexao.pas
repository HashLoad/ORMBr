unit UDM_Conexao;

interface

uses
  System.SysUtils,
  System.Classes,

  //ORMBr
  ormbr.factory.interfaces,
  ormbr.factory.firedac,
  Data.DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.Oracle, FireDAC.Phys.OracleDef,
  FireDAC.VCLUI.Wait, FireDAC.Comp.Client, FireDAC.Phys.PG, FireDAC.Phys.PGDef,
  FireDAC.Phys.FB, FireDAC.Phys.FBDef;

type
  TDataModule1 = class(TDataModule)
    fdconnFB: TFDConnection;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule1: TDataModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.

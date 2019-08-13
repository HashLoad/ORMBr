unit uMainFormORM;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  DB,
  Grids,
  DBGrids,
  StdCtrls,
  Mask,
  DBClient,
  DBCtrls,
  ExtCtrls,
  /// orm model
  ormbr.model.master,
  ormbr.model.detail,
  ormbr.model.lookup,
  ormbr.model.client,
  ormbr.container.dataset.interfaces,
  ormbr.container.restfdmemtable,
  ormbr.session.baseurl,

  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TForm3 = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Button2: TButton;
    DBGrid2: TDBGrid;
    DataSource2: TDataSource;
    DataSource3: TDataSource;
    DBEdit1: TDBEdit;
    Label1: TLabel;
    Label2: TLabel;
    DBEdit2: TDBEdit;
    Label3: TLabel;
    DBEdit3: TDBEdit;
    Label4: TLabel;
    DBEdit4: TDBEdit;
    Label5: TLabel;
    DBEdit5: TDBEdit;
    Label6: TLabel;
    DBEdit6: TDBEdit;
    Label7: TLabel;
    FDMaster: TFDMemTable;
    FDDetail: TFDMemTable;
    FDClient: TFDMemTable;
    FDLookup: TFDMemTable;
    Label8: TLabel;
    DBEdit7: TDBEdit;
    Button1: TButton;
    DBImage1: TDBImage;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    oDataSnapMaster: IContainerDataSet<Tmaster>;
    oDataSnapDetail: IContainerDataSet<Tdetail>;
    oDataSnapClient: IContainerDataSet<Tclient>;
    oDataSnapLookup: IContainerDataSet<Tlookup>;
public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  StrUtils,
  Generics.Collections,
  ormbr.rest.json;

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
var
  LMasterCurrent: Tmaster;
begin
  LMasterCurrent := oDataSnapMaster.Current;
  LMasterCurrent.description := 'Registro Alterado Pelo Object';
  LMasterCurrent.registerdate := Now;
  oDataSnapMaster.Save(LMasterCurrent);
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  oDataSnapMaster.ApplyUpdates(0);
end;

procedure TForm3.FormCreate(Sender: TObject);
var
  LMasterList: TObjectList<Tmaster>;
  LFor: Integer;
begin
  /// <summary>
  /// Definir URL base na classe sington que será usada pelo ORMBr internamente a sessão.
  /// </summary>
  TSessionRESTBaseURL.GetInstance.BaseURL := 'http://127.0.0.1:211/datasnap/rest/tormbr';
  // Master
  oDataSnapMaster := TContainerRESTFDMemTable<Tmaster>.Create(FDMaster);
  // Detail
  oDataSnapDetail := TContainerRESTFDMemTable<Tdetail>.Create(FDDetail, oDataSnapMaster.This);
  // Client
  oDataSnapClient := TContainerRESTFDMemTable<Tclient>.Create(FDClient, oDataSnapMaster.This);
  // DBComboBoxLookup
  oDataSnapLookup := TContainerRESTFDMemTable<Tlookup>.Create(FDLookup);
  oDataSnapDetail.AddLookupField('fieldname',
                                 'lookup_id',
                                 oDataSnapLookup.This,
                                 'lookup_id',
                                 'lookup_description',
                                 'Descrição Lookup');

  oDataSnapMaster.Open;
//  oDataSnapMaster.Open(74);
//  oDataSnapMaster.OpenWhere('master_id = 74');

  LMasterList := oDataSnapMaster.Find;
  try
    Memo1.Lines.Text := TORMBrJSON.ObjectListToJsonString<Tmaster>(LMasterList);
  finally
    LMasterList.Free;
  end;
end;

end.

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
  Rtti,
  /// orm factory
  dbebr.factory.interfaces,
  /// orm injection dependency
  ormbr.container.dataset.interfaces,
  ormbr.container.fdmemtable,
  ormbr.container.clientdataset,
  ormbr.container.objectset.interfaces,
  ormbr.container.objectset,
  dbebr.factory.firedac.mongodb,
  ormbr.json,
  /// orm model
  ormbr.dml.generator.mongodb,
  ormbr.model.client,

  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.MongoDB,
  FireDAC.Phys.MongoDBDef, FireDAC.Phys.MongoDBWrapper,
  FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Comp.UI, System.JSON.Types,
  System.JSON.Readers, System.JSON.BSON, System.JSON.Builders;

type
  TForm3 = class(TForm)
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Button2: TButton;
    DataSource3: TDataSource;
    Label3: TLabel;
    DBEdit3: TDBEdit;
    Label4: TLabel;
    DBEdit4: TDBEdit;
    FDConnection1: TFDConnection;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDClient: TFDMemTable;
    Button1: TButton;
    FDPhysMongoDriverLink1: TFDPhysMongoDriverLink;
    DataSource1: TDataSource;
    DBGrid2: TDBGrid;
    ClientDataSet1: TClientDataSet;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    oConn: IDBConnection;
    oContainerClient: IContainerDataSet<Tclient>;
    oContainerObject: IContainerObjectSet<Tclient>;
public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  StrUtils,
  Generics.Collections, dbcbr.mapping.register, dbcbr.mapping.explorer;

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
var
  LClient: Tclient;
begin
  LClient := oContainerClient.Current;
  LClient.client_name := 'Mudar campo "Nome" pelo objeto';
  oContainerClient.Save(LClient);
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  oContainerClient.ApplyUpdates(0);
end;

procedure TForm3.FormCreate(Sender: TObject);
var
  LClientList: TObjectList<Tclient>;
  I: Integer;
begin
  oConn := TFactoryMongoFireDAC.Create(FDConnection1, dnMongoDB);

  // Client
  oContainerClient := TContainerFDMemTable<Tclient>.Create(oConn, FDClient);
//  oContainerClient := TContainerClientDataSet<Tclient>.Create(oConn, ClientDataSet1);
  oContainerClient.Open;
//  oContainerClient.OpenWhere('{"client_id": 2}', '');

  // Campo do tipo TDataSetField que recebe os sub-objects da coleção selecionada
  // Será criado um campo desse tipo para cada sub-object, veja como definir o tipo
  // no modelo na unit ormbr.model.client.pas que segue junto como o exemplo.
  DataSource1.DataSet := (FDClient.FieldByName('address') as TDataSetField).NestedDataSet;
//  DataSource1.DataSet := (ClientDataSet1.FieldByName('address') as TDataSetField).NestedDataSet;

  oContainerObject := TContainerObjectSet<Tclient>.Create(oConn);

  // Somente demonstração de funcionalidade
//  LClientList := oContainerObject.Find;
  LClientList := oContainerObject.FindWhere('{"client_id": 2}', '');
  /// Converte lista para JSON
  try
    Memo1.Lines.Text := TORMBrJson.ObjectListToJsonString<Tclient>(LClientList);
  finally
    LClientList.Free;
  end;
  /// Converte JSON para lista
  try
    LClientList := TORMBrJson.JsonToObjectList<Tclient>(Memo1.Lines.Text);
  finally
    LClientList.Free;
  end;
end;

end.

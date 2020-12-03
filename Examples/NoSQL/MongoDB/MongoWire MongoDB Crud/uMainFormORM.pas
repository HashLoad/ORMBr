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
  ormbr.container.clientdataset,
  ormbr.container.objectset.interfaces,
  ormbr.container.objectset,
  dbebr.factory.wire.mongodb,
  ormbr.json,
  ormbr.rest.json,
  /// orm model
  ormbr.model.client,

  JSON.Types,
  JSON.Readers,
  JSON.BSON,
  JSON.Builders, MongoWireConnection;

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
    CDSClient: TClientDataSet;
    Button1: TButton;
    DataSource1: TDataSource;
    DBGrid2: TDBGrid;
    ClientDataSet1: TClientDataSet;
    Memo1: TMemo;
    MongoWireConnection1: TMongoWireConnection;
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
  Generics.Collections, ormbr.mapping.register, ormbr.mapping.explorer;

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
  // Instância da class de conexão via FireDAC
  oConn := TFactoryMongoWire.Create(MongoWireConnection1, dnMongoDB);

  // Client
  oContainerClient := TContainerClientDataSet<Tclient>.Create(oConn, CDSClient);
  oContainerClient.Open;
//  oContainerClient.OpenWhere('{"client_id": 2}', '');

  // Campo do tipo TDataSetField que recebe os sub-objects da coleção selecionada
  // Será criado um campo desse tipo para cada sub-object, veja como definir o tipo
  // no modelo na unit ormbr.model.client.pas que segue junto como o exemplo.
  DataSource1.DataSet := (CDSClient.FieldByName('address') as TDataSetField).NestedDataSet;

  oContainerObject := TContainerObjectSet<Tclient>.Create(oConn);

  // Somente demonstração de funcionalidade
  LClientList := oContainerObject.Find;
//  LClientList := oContainerObject.FindWhere('{"client_id": 2}', '');
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

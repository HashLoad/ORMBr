unit Client.Forms.Main;

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
  /// ORMBr Modelos
  ormbr.model.master,
  ormbr.model.detail,
  ormbr.model.lookup,
  ormbr.model.client,
  /// ORMBr Manager
  ormbr.manager.dataset,
  ormbr.manager.objectset,
  ormbr.client,
  ormbr.client.base,
  ormbr.client.wirl,
  ormbr.client.methods,

  /// FireDAC
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client,

  /// WiRL
  WiRL.http.Request, WiRL.http.Client, WiRL.http.Response;

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
    Button3: TButton;
    Button4: TButton;
    RESTClientWiRL1: TRESTClientWiRL;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    oDTSManager: TManagerDataSet;
    oOBJManager: TManagerObjectSet;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  StrUtils,
  Generics.Collections,
  ormbr.form.monitor,
  ormbr.rest.json;

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
var
  LCurrent: Tmaster;
begin
  LCurrent := oDTSManager.Current<Tmaster>;
  LCurrent.description := 'Object Update Master';
  oDTSManager.Save<Tmaster>(LCurrent);
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  oDTSManager.ApplyUpdates<Tmaster>(0);
end;

procedure TForm3.Button3Click(Sender: TObject);
begin
  TCommandMonitor.GetInstance.Show;
end;

procedure TForm3.Button4Click(Sender: TObject);
begin
  oDTSManager.RefreshRecord<Tmaster>;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  RESTClientWiRL1
    .AsConnection
      .SetCommandMonitor(TCommandMonitor.GetInstance);

  oDTSManager := TManagerDataSet.Create(RESTClientWiRL1.AsConnection);
  oDTSManager.AddAdapter<Tmaster>(FDMaster, 5)
             .AddAdapter<Tdetail, Tmaster>(FDDetail)
             .AddAdapter<Tclient, Tmaster>(FDClient)
             .AddAdapter<Tlookup>(FDLookup)
             .AddLookupField<Tdetail, Tlookup>('fieldname',
                                               'lookup_id',
                                               'lookup_id',
                                               'lookup_description',
                                               'Descrição Lookup')
//  .Open<Tmaster>;
  .OpenWhere<Tmaster>('description like ''IB%''');

//  oOBJManager := TManagerObjectSet.Create(RESTClientWiRL1.AsConnection);
//  oOBJManager.AddAdapter<Tmaster>(2);
//  oOBJManager.Find<Tmaster>;

//  oOBJManager.FindWhere<Tmaster>('master_id = 6');
//  if oOBJManager.NestedList<Tmaster>.Count > 0 then
//  begin
//    oOBJManager.NestedList<Tmaster>.First;
//    for LMaster in oOBJManager.NestedList<Tmaster> do
//    begin
//      if LMaster <> nil then
//        Memo1.Lines.Add(TORMBrJson.ObjectToJsonString(LMaster));
//    end;
//  end;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  oOBJManager.Free;
  oDTSManager.Free;
end;

end.

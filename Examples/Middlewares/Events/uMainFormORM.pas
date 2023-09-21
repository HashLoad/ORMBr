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
  Generics.Collections,
  /// orm factory
  dbebr.factory.interfaces,
  /// orm injection dependency
  dbebr.factory.firedac,
  ormbr.container.objectset,
  ormbr.container.objectset.interfaces,
  ormbr.dml.generator.sqlite,
  /// orm model
  ormbr.model.master,
  ormbr.model.detail,
  ormbr.model.lookup,
  ormbr.model.client,

  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.VCLUI.Wait, FireDAC.Comp.Client, FireDAC.Stan.Intf,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  FireDAC.Comp.UI, FireDAC.DApt, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Phys.SQLiteWrapper.Stat;

type
  TStringGridHack = class(TStringGrid)
  protected
    procedure DeleteRow(ARow: Longint); reintroduce;
    procedure InsertRow(ARow: Longint);
  end;

  TForm3 = class(TForm)
    btnOpen: TButton;
    Label7: TLabel;
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    StringGridMaster: TStringGrid;
    StringGridDetail: TStringGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    edtMaster_Descricao: TEdit;
    edtClient_ID: TEdit;
    edtClient_Nome: TEdit;
    edtMaster_Cadastro: TEdit;
    edtMaster_Alteracao: TEdit;
    Edit7: TEdit;
    Label8: TLabel;
    Button1: TButton;
    edtMaster_ID: TEdit;
    btnInsert: TButton;
    btnDelete: TButton;
    btnUpdate: TButton;
    Button2: TButton;
    imgClient_Foto: TImage;
    procedure btnOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure StringGridMasterSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure btnInsertClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnUpdateClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    oConn: IDBConnection;
    oMaster: IContainerObjectSet<Tmaster>;
    oMasterList: TObjectList<Tmaster>;
    procedure MasterStringGridFill(AMasterList: TObjectList<Tmaster>; AIndex: Integer = 0);
    procedure MasterStringGridDefinitions;
    procedure MasterStinggGridAddRow(AObject: Tmaster);
    procedure SetValuesEdits(AIndex: Integer);
    procedure ClearValueEdits;
    procedure DetailStringGridFill(ADetailList: TObjectList<Tdetail>);
    procedure DetailStringGridDefinitions;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  StrUtils,
  ormbr.form.monitor,
  dbcbr.mapping.explorer,
  ormbr.objects.helper,
  ormbr.json,
  ormbr.rtti.helper;

{$R *.dfm}

procedure TForm3.FormCreate(Sender: TObject);
var
  CanSelect: Boolean;
begin
  MasterStringGridDefinitions;
  DetailStringGridDefinitions;

  oConn := TFactoryFireDAC.Create(FDConnection1,
                                  dnSQLite,
                                  TCommandMonitor.GetInstance);
  oMaster := TContainerObjectSet<Tmaster>.Create(oConn, 10);
  oMasterList := oMaster.Find;
  // LINHA ABAIXO SUBSTITUIDO PELO Middleware, OLHAR NA CLASSE TMaster (ormbr.model.master.pas)
  // oMasterList := oMaster.FindWhere('master_id > 0 and master_id <> 6');

  // Preenche o grid
  MasterStringGridFill(oMasterList);
  // Dispara o evento onde abre as sub-tabelas
  StringGridMaster.OnSelectCell(StringGridMaster, 0, 1, CanSelect);
end;

procedure TForm3.Button1Click(Sender: TObject);
var
  LIndex: Integer;
begin
  // Busca o proximo pacote de registros
  LIndex := oMasterList.Count;
  oMaster.NextPacket(oMasterList);
  MasterStringGridFill(oMasterList, LIndex);
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  TCommandMonitor.GetInstance.Show;
end;

procedure TForm3.btnOpenClick(Sender: TObject);
begin
  MasterStringGridFill(oMaster.Find);
end;

procedure TForm3.btnUpdateClick(Sender: TObject);
var
oMasterUpd: Tmaster;
begin
  // Faz uma cópia do objeto antes dele sofre as mudanças necessárias
  oMasterUpd := oMasterList.Items[StringGridMaster.Row-1];
  oMaster.Modify(oMasterUpd);
  oMasterUpd.description := edtMaster_Descricao.Text;
  oMasterUpd.registerdate := StrToDate(edtMaster_Cadastro.Text);
  oMasterUpd.updatedate := StrToDate(edtMaster_Alteracao.Text);
  //
  oMasterUpd.detail.Add(Tdetail.Create);
  oMasterUpd.detail.Last.detail_id := oMasterUpd.detail.Count + 1;
  oMasterUpd.detail.Last.master_id := oMasterUpd.master_id;
  oMasterUpd.detail.Last.lookup_id := 1;
  oMasterUpd.detail.Last.price := 556.88;
  oMasterUpd.detail.Last.lookup_description := 'UDATE VIA CASDACE';

  // Altera o registro no Banco
  oMaster.Update(oMasterUpd);
end;

procedure TForm3.btnInsertClick(Sender: TObject);
var
oMasterNew: Tmaster;
begin
  oMasterNew := Tmaster.Create;
  oMasterNew.SetDefaultValue;
//(foi definido no Dictionary() da classe como default)
//  oMasterNew.master_id := -1;
  oMasterNew.description := edtMaster_Descricao.Text;
  oMasterNew.client_id := StrToInt(edtClient_ID.Text);
  oMasterNew.registerdate := StrToDate(edtMaster_Cadastro.Text);
  oMasterNew.updatedate := StrToDate(edtMaster_Alteracao.Text);
  oMasterNew.client_name := edtClient_Nome.Text;
  // Child
  oMasterNew.detail.Add(Tdetail.Create);
//(foi definido no Dictionary() da classe como default)
//  oMasterNew.detail.Last.master_id := -1;
  oMasterNew.detail.Last.detail_id := 1;
  oMasterNew.detail.Last.lookup_id := 2;
  oMasterNew.detail.Last.lookup_description := 'Insert Cascade 1';
  oMasterNew.detail.Last.price := 165.78;
  // Child
  oMasterNew.detail.Add(Tdetail.Create);
//(foi definido no Dictionary() da classe como default)
//  oMasterNew.detail.Last.master_id := -1;
  oMasterNew.detail.Last.detail_id := 2;
  oMasterNew.detail.Last.lookup_id := 3;
  oMasterNew.detail.Last.lookup_description := 'Insert Cascade 2';
  oMasterNew.detail.Last.price := 333.78;
  // Insere o registro no Banco
  oMaster.Insert(oMasterNew);
  // Adiciona na lista
  oMasterList.Add(oMasterNew);
  // Repassa os dados para o grid
  MasterStinggGridAddRow(oMasterNew);
  // Adiciona nova linha no grid
  TStringGridHack(StringGridMaster).InsertRow(1);
end;

procedure TForm3.btnDeleteClick(Sender: TObject);
var
oMasterDel: Tmaster;
begin
  oMasterDel := oMasterList.Items[StringGridMaster.Row-1];
  try
    oMaster.Delete(oMasterDel);
    oMasterList.Delete(StringGridMaster.Row-1);
    TStringGridHack(StringGridMaster).DeleteRow(StringGridMaster.Row);
  finally
  end;
end;

procedure TForm3.ClearValueEdits;
begin
  edtMaster_ID.Clear;
  edtMaster_Descricao.Clear;
  edtClient_ID.Clear;
  edtClient_Nome.Clear;
  edtMaster_Cadastro.Clear;
  edtMaster_Alteracao.Clear;
end;

procedure TForm3.DetailStringGridDefinitions;
var
  iFor: Integer;
begin
  StringGridDetail.ColCount := 5;

  for iFor := 0 to StringGridDetail.ColCount -1 do
    StringGridDetail.ColWidths[iFor] := 150;

  StringGridDetail.Cols[0].Text := 'Detail_ID';
  StringGridDetail.Cols[1].Text := 'Master_ID';
  StringGridDetail.Cols[2].Text := 'Lookup_ID';
  StringGridDetail.Cols[3].Text := 'Lookup_Descricao';
  StringGridDetail.Cols[4].Text := 'Preço Unitário';
end;

procedure TForm3.DetailStringGridFill(ADetailList: TObjectList<Tdetail>);
var
  LDetail: Tdetail;
  LPrice: string;
begin
  StringGridDetail.RowCount := 1;
  for LDetail in ADetailList do
  begin
    StringGridDetail.Cells[0, StringGridDetail.RowCount] := IntToStr(LDetail.detail_id);
    StringGridDetail.Cells[1, StringGridDetail.RowCount] := IntToStr(LDetail.master_id);
    StringGridDetail.Cells[2, StringGridDetail.RowCount] := IntToStr(LDetail.lookup_id);
    StringGridDetail.Cells[3, StringGridDetail.RowCount] := LDetail.lookup_description;
    LPrice := FormatFloat('#,###,##0.00', LDetail.price);
    StringGridDetail.Cells[4, StringGridDetail.RowCount] := StringOfChar(' ', 43 - Length(LPrice)) + LPrice;
    TStringGridHack(StringGridDetail).InsertRow(1);
  end;
  if StringGridDetail.RowCount > 1 then
    StringGridDetail.FixedRows := 1;
end;

procedure TForm3.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  oMasterList.Clear;
  oMasterList.Free;
end;

procedure TForm3.SetValuesEdits(AIndex: Integer);
begin
  edtMaster_ID.Text        := IntToStr(oMasterList.Items[AIndex -1].master_id);
  edtMaster_Descricao.Text := oMasterList.Items[AIndex -1].description;
  edtMaster_Cadastro.Text  := DateTimeToStr(oMasterList.Items[AIndex -1].registerdate);
  edtMaster_Alteracao.Text := DateTimeToStr(oMasterList.Items[AIndex -1].updatedate);
  edtClient_ID.Text        := IntToStr(oMasterList.Items[AIndex -1].client_id);
  edtClient_Nome.Text      := oMasterList.Items[AIndex -1].client_name;
  // Mostra a imagem no imgClient_Foto em tela.
//  oMasterList.Items[AIndex -1].client.client_foto.ToPicture(imgClient_Foto.Picture);
end;

procedure TForm3.StringGridMasterSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  if ARow > 0 then
  begin
    // Preenche os edites com a linha selecionada no grid
    SetValuesEdits(ARow);
    // Preenche a lista de detalhes
    DetailStringGridFill(oMasterList[ARow -1].detail);
    ///
    Edit7.Text := FormatFloat('###,##0.00', oMasterList[ARow -1].total);
  end;
end;

procedure TForm3.MasterStinggGridAddRow(AObject: Tmaster);
begin
  StringGridMaster.Cells[0, StringGridMaster.RowCount] := IntToStr(AObject.master_id);
  StringGridMaster.Cells[1, StringGridMaster.RowCount] := AObject.description;
  StringGridMaster.Cells[2, StringGridMaster.RowCount] := DateTimeToStr(AObject.registerdate);
  StringGridMaster.Cells[3, StringGridMaster.RowCount] := DateTimeToStr(AObject.updatedate);
  StringGridMaster.Cells[4, StringGridMaster.RowCount] := IntToStr(AObject.client_id);
  StringGridMaster.Cells[5, StringGridMaster.RowCount] := AObject.client.client_name;
end;

procedure TForm3.MasterStringGridDefinitions;
var
  iFor: Integer;
begin
  StringGridMaster.ColCount := 6;

  for iFor := 0 to StringGridMaster.ColCount -1 do
    StringGridMaster.ColWidths[iFor] := 150;

  StringGridMaster.Cols[0].Text := 'ID';
  StringGridMaster.Cols[1].Text := 'Descrição';
  StringGridMaster.Cols[2].Text := 'Data Cadastro';
  StringGridMaster.Cols[3].Text := 'Data Alteração';
  StringGridMaster.Cols[4].Text := 'Cliente ID';
  StringGridMaster.Cols[5].Text := 'Cliente Nome';
end;

procedure TForm3.MasterStringGridFill(AMasterList: TObjectList<Tmaster>;
  AIndex: Integer);
var
  LFor: Integer;
begin
  for LFor := AIndex to AMasterList.Count -1 do
  begin
    // Adiciona a lista de objetos geral.
    MasterStinggGridAddRow(AMasterList.Items[LFor]);
    TStringGridHack(StringGridMaster).InsertRow(1);
  end;
  if StringGridMaster.RowCount > 1 then
    StringGridMaster.FixedRows := 1;
  //
  SetValuesEdits(1);
end;

{ TStringGridHack }

procedure TStringGridHack.DeleteRow(ARow: Longint);
var
  GemRow: Integer;
begin
  GemRow := Row;
  if RowCount > FixedRows + 1 then
    inherited DeleteRow(ARow)
  else
    Rows[ARow].Clear;
  if GemRow < RowCount then
    Row := GemRow;
end;

procedure TStringGridHack.InsertRow(ARow: Longint);
var
  GemRow: Integer;
begin
  GemRow := Row;
  while ARow < FixedRows do
    Inc(ARow);
  RowCount := RowCount + 1;
  Row := GemRow;
end;

end.

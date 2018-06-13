unit Tests.ClientDataset;

interface
uses
  System.SysUtils,
  DUnitX.TestFramework,
  Data.SqlExpr,
  Data.DbxSqlite,
  Datasnap.DBClient,
  Data.DB,
  ormbr.factory.interfaces,
  ormbr.factory.dbexpress,
  ormbr.dependency.interfaces,
  ormbr.dependency.injection.clientdataset,
  ormbr.model.master,
  ormbr.types.database,
  Helper.Dataset;

type

  [TestFixture]
  TClientDatasetTests = class(TObject)
  private
    FSQLConnection    : TSQLConnection;
    FConnection       : IDBConnection;
    FDataSetMasterDemo: TClientDataSet;
    FModelMasterDemo  : IContainerDataSet<Tmaster>;
    FRowCountTest     : Integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure ClientDatasetApplyUpdatesTest;
  end;

implementation

procedure TClientDatasetTests.ClientDatasetApplyUpdatesTest;
begin
  try
    FDataSetMasterDemo  := TClientDataSet.Create(nil);
    FModelMasterDemo    := TContainerClientDataSet<Tmaster>.Create(FConnection, FDataSetMasterDemo);

    FModelMasterDemo.DataSet.Open;
    FRowCountTest := FDataSetMasterDemo.RecordCount + 1;

    FDataSetMasterDemo.Append;
    FDataSetMasterDemo.FieldByName('master_Id').AsInteger     := FRowCountTest;
    FDataSetMasterDemo.FieldByName('description').AsString    := 'Master Demo Test ' + IntToStr(FRowCountTest);
    FDataSetMasterDemo.FieldByName('updatedate').AsDateTime   := Date;
    FDataSetMasterDemo.FieldByName('registerdate').AsDateTime := Date;
    FDataSetMasterDemo.FieldByName('client_id').AsInteger     := 1;
    FDataSetMasterDemo.Post;

    FModelMasterDemo.DataSet.ApplyUpdates(0);

    FModelMasterDemo.DataSet.Close;
    FModelMasterDemo.DataSet.Open;

    Assert.AreEqual(FRowCountTest, FDataSetMasterDemo.RecordCount, 'RowCount <> RecordCount');
  finally
    FreeAndNil(FDataSetMasterDemo);
  end;
end;

procedure TClientDatasetTests.Setup;
begin
  FSQLConnection := TSQLConnection.Create(nil);
  FSQLConnection.KeepConnection := False;
  FSQLConnection.LoadParamsOnConnect := False;
  FSQLConnection.LoginPrompt := False;
  FSQLConnection.Params.Clear;
  FSQLConnection.DriverName := 'Sqlite';
  FSQLConnection.Params.Add('Database=..\Demo\Data\Database\database.db3');
  FSQLConnection.Params.Add('FailIfMissing=True');

  FConnection := TFactoryDBExpress.Create(FSQLConnection, dnSQLite);
end;

procedure TClientDatasetTests.TearDown;
begin
  if Assigned(FSQLConnection) then
    FreeAndNil(FSQLConnection);
end;


initialization
  TDUnitX.RegisterTestFixture(TClientDatasetTests);
end.

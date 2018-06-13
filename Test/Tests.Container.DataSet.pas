unit Tests.Container.DataSet;

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
  TContainerDataSetTests = class(TObject)
  private
    FSQLConnection    : TSQLConnection;
    FConnection       : IDBConnection;
    FModelMasterDemo  : IContainerDataSet<Tmaster>;
    FDataSetMasterDemo: TClientDataSet;
    FRowCountTest     : Integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure ContainerDataSetApplyUpdatesTest;
  end;

implementation

procedure TContainerDataSetTests.ContainerDataSetApplyUpdatesTest;
begin
   try
      FDataSetMasterDemo  := TClientDataSet.Create(nil);
      FModelMasterDemo    := TContainerClientDataSet<Tmaster>.Create(FConnection, FDataSetMasterDemo);

      FModelMasterDemo.DataSet.Open;
      FRowCountTest := FDataSetMasterDemo.RecordCount + 1;

      FModelMasterDemo.DataSet.Append;
      FModelMasterDemo.DataSet.Current.master_Id := FRowCountTest;
      FModelMasterDemo.DataSet.Current.description := 'Master Demo Test ' + IntToStr(FRowCountTest);
      FModelMasterDemo.DataSet.Current.updatedate := Date;
      FModelMasterDemo.DataSet.Current.registerdate := Date;
      FModelMasterDemo.DataSet.Current.client_id := 1;
      FModelMasterDemo.DataSet.Post;

      FModelMasterDemo.DataSet.ApplyUpdates(0);

      FModelMasterDemo.DataSet.Close;
      FModelMasterDemo.DataSet.Open;

      Assert.AreEqual(FRowCountTest, FDataSetMasterDemo.RecordCount, 'RowCount <> RecordCount');
    finally
      FreeAndNil(FDataSetMasterDemo);
    end;
end;

procedure TContainerDataSetTests.Setup;
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

procedure TContainerDataSetTests.TearDown;
begin
  if Assigned(FSQLConnection) then
    FreeAndNil(FSQLConnection);
end;


initialization
  TDUnitX.RegisterTestFixture(TContainerDataSetTests);
end.

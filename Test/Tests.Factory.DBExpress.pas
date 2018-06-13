unit Tests.Factory.DBExpress;

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
  ormbr.criteria,
  ormbr.model.master,
  ormbr.model.detail,
  ormbr.model.lookup,
  ormbr.model.client,
  ormbr.types.database,
  Helper.Dataset;

type

  [TestFixture]
  TFactoryDBExpressTests = class(TObject)
  private
    FSQLConnection    : TSQLConnection;
    FConnection       : IDBConnection;
    FIDBQuery         : IDBQuery;
    FModelClientDemo  : IContainerDataSet<Tclient>;
    FDataSetClientDemo: TClientDataset;
    FRowCountTest     : Integer;
    const
      SQL_INSERT_TEST = ' INSERT INTO client (client_id, client_name) VALUES (%d, %s); ';
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure FactoryDBExpressIDBConnectionConnectTest;
    [Test]
    procedure FactoryDBExpressIDBConnectionDisconnectTest;
    [Test]
    procedure FactoryDBExpressIDBConnectionAndTSQLConnectionConnectTest;
    [Test]
    procedure FactoryDBExpressIDBConnectionAndTSQLConnectionDisconnectTest;
    [Test]
    procedure FactoryDBExpressInTransactionTest;
    [Test]
    procedure FactoryDBExpressIDBConnectionAndTSQLConnectionInTransactionTest;
    [Test]
    procedure FactoryDBExpressCommitTest;
    [Test]
    procedure FactoryDBExpressRollbackTest;
    [Test]
    procedure FactoryDBExpressExecuteDirectTest;
  end;

implementation

procedure TFactoryDBExpressTests.FactoryDBExpressInTransactionTest;
begin
  FConnection.Connect;
  FConnection.StartTransaction;
  Assert.IsTrue(FConnection.InTransaction, 'FConnection.InTransaction = false');
end;

procedure TFactoryDBExpressTests.FactoryDBExpressRollbackTest;
var
  oKeyValue : Integer;
begin
  try
    FDataSetClientDemo := TClientDataSet.Create(nil);
    FModelClientDemo   := TContainerClientDataSet<Tclient>.Create(FConnection, FDataSetClientDemo);

    FModelClientDemo.DataSet.Open;
    oKeyValue     := FDataSetClientDemo.GetNextKeyValue('client_id') + 1;
    FRowCountTest := FDataSetClientDemo.RecordCount + 1;

    FConnection.StartTransaction;

    FIDBQuery := FConnection.CreateQuery;
    FIDBQuery.CommandText := Format(SQL_INSERT_TEST,[oKeyValue, QuotedStr('DBExpress RollbackTest ' + oKeyValue.ToString)]);
    FIDBQuery.ExecuteDirect;

    if FConnection.InTransaction then
      FConnection.Rollback;

    FModelClientDemo.DataSet.Close;
    FModelClientDemo.DataSet.Open;

    Assert.AreNotEqual(FRowCountTest, FDataSetClientDemo.RecordCount, '');
  finally
    FreeAndNil(FDataSetClientDemo);
  end;
end;

procedure TFactoryDBExpressTests.Setup;
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

procedure TFactoryDBExpressTests.TearDown;
begin
  if Assigned(FSQLConnection) then
    FreeAndNil(FSQLConnection);
end;

procedure TFactoryDBExpressTests.FactoryDBExpressCommitTest;
var
  oKeyValue : Integer;
begin
  try
    FDataSetClientDemo := TClientDataSet.Create(nil);
    FModelClientDemo   := TContainerClientDataSet<Tclient>.Create(FConnection, FDataSetClientDemo);

    FModelClientDemo.DataSet.Open;
    oKeyValue     := FDataSetClientDemo.GetNextKeyValue('client_id') + 1;
    FRowCountTest := FDataSetClientDemo.RecordCount + 1;

    FConnection.StartTransaction;

    FIDBQuery := FConnection.CreateQuery;
    FIDBQuery.CommandText := Format(SQL_INSERT_TEST,[oKeyValue, QuotedStr('DBExpress CommitTest ' + oKeyValue.ToString)]);
    FIDBQuery.ExecuteDirect;

    if FConnection.InTransaction then
      FConnection.Commit;

    FModelClientDemo.DataSet.Close;
    FModelClientDemo.DataSet.Open;

    Assert.AreEqual(FRowCountTest, FDataSetClientDemo.RecordCount, '');
  finally
    FreeAndNil(FDataSetClientDemo);
  end;
end;

procedure TFactoryDBExpressTests.FactoryDBExpressExecuteDirectTest;
var
  sMasterUpdatedateValue,
  sMasterUpdatedateNewValue: string;
begin
  FConnection.Connect;

  FConnection.StartTransaction;
  sMasterUpdatedateNewValue := QuotedStr(FormatDateTime('yyyy-mm-dd', Now));
  FConnection.ExecuteDirect(CreateCriteria.Update('master').&Set('updatedate', sMasterUpdatedateNewValue).Where('master_id = 1').AsString);
  FConnection.Commit;

  FIDBQuery := FConnection.CreateQuery;
  FIDBQuery.CommandText := CreateCriteria.Select.All.From('master').Where('master_id = 1').AsString;
  sMasterUpdatedateValue := FIDBQuery.ExecuteQuery.GetFieldValue('updatedate');

  Assert.AreEqual(sMasterUpdatedateNewValue, QuotedStr(sMasterUpdatedateValue), sMasterUpdatedateNewValue + ' <> ' + QuotedStr(sMasterUpdatedateValue));
end;

procedure TFactoryDBExpressTests.FactoryDBExpressIDBConnectionAndTSQLConnectionConnectTest;
begin
  FConnection.Connect;
  Assert.AreEqual(FConnection.IsConnected, FSQLConnection.Connected, 'FConnection.IsConnected <> FSQLConnection.Connected');
end;

procedure TFactoryDBExpressTests.FactoryDBExpressIDBConnectionAndTSQLConnectionDisconnectTest;
begin
  FConnection.Connect;
  FConnection.Disconnect;
  Assert.AreEqual(FConnection.IsConnected, FSQLConnection.Connected, 'FConnection.IsConnected <> FSQLConnection.Connected');
end;

procedure TFactoryDBExpressTests.FactoryDBExpressIDBConnectionAndTSQLConnectionInTransactionTest;
begin
  FConnection.Connect;
  FConnection.StartTransaction;
  Assert.AreEqual(FConnection.InTransaction, FSQLConnection.InTransaction, 'FConnection.InTransaction <> FSQLConnection.InTransaction');
end;

procedure TFactoryDBExpressTests.FactoryDBExpressIDBConnectionConnectTest;
begin
  FConnection.Connect;
  Assert.IsTrue(FConnection.IsConnected, 'FConnection.IsConnected = false');
end;


procedure TFactoryDBExpressTests.FactoryDBExpressIDBConnectionDisconnectTest;
begin
  FConnection.Connect;
  FConnection.Disconnect;
  Assert.IsFalse(FConnection.IsConnected, 'FConnection.IsConnected = true');
end;

initialization
  TDUnitX.RegisterTestFixture(TFactoryDBExpressTests);
end.

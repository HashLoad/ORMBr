unit Tests.Factory.Zeos;

interface
uses
  DUnitX.TestFramework,
  System.SysUtils,
  ZConnection,
  ZDbcIntfs,
  ormbr.factory.interfaces,
  ormbr.factory.zeos,
  Datasnap.DBClient,
  Data.DB,
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
  TFactoryZeosTests = class(TObject)
  private
    FZConnection      : TZConnection;
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
    procedure FactoryZeosIDBConnectionConnectTest;
    [Test]
    procedure FactoryZeosIDBConnectionDisconnectTest;
    [Test]
    procedure FactoryZeosIDBConnectionAndTZConnectionConnectTest;
    [Test]
    procedure FactoryZeosIDBConnectionAndTZConnectionDisconnectTest;
    [Test]
    procedure FactoryZeosInTransactionTest;
    [Test]
    procedure FactoryZeosIDBConnectionAndTZConnectionInTransactionTest;
    [Test]
    procedure FactoryZeosCommitTest;
    [Test]
    procedure FactoryZeosRollbackTest;
    [Test]
    procedure FactoryZeosExecuteDirectTest;
  end;

implementation

procedure TFactoryZeosTests.FactoryZeosInTransactionTest;
begin
  FConnection.Connect;
  FConnection.StartTransaction;
  Assert.IsTrue(FConnection.InTransaction, 'FConnection.InTransaction = false');
end;

procedure TFactoryZeosTests.FactoryZeosRollbackTest;
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
    FIDBQuery.CommandText := Format(SQL_INSERT_TEST,[oKeyValue, QuotedStr('Zeos RollbackTest ' + oKeyValue.ToString)]);
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

procedure TFactoryZeosTests.Setup;
begin
  FZConnection := TZConnection.Create(nil);
  FZConnection.LoginPrompt := False;
  FZConnection.Protocol := 'sqlite-3';
  FZConnection.Database := '..\Demo\Data\Database\database.db3';
  FZConnection.TransactIsolationLevel := tiReadUncommitted;

  FConnection := TFactoryZeos.Create(FZConnection, dnSQLite);
end;

procedure TFactoryZeosTests.TearDown;
begin
  if Assigned(FZConnection) then
    FreeAndNil(FZConnection);
end;

procedure TFactoryZeosTests.FactoryZeosCommitTest;
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
    FIDBQuery.CommandText := Format(SQL_INSERT_TEST,[oKeyValue, QuotedStr('Zeos CommitTest ' + oKeyValue.ToString)]);
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

procedure TFactoryZeosTests.FactoryZeosExecuteDirectTest;
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
  sMasterUpdatedateValue := FormatDateTime('yyyy-mm-dd', FIDBQuery.ExecuteQuery.GetFieldValue('updatedate'));

  Assert.AreEqual(sMasterUpdatedateNewValue, QuotedStr(sMasterUpdatedateValue), sMasterUpdatedateNewValue + ' <> ' + QuotedStr(sMasterUpdatedateValue));
end;

procedure TFactoryZeosTests.FactoryZeosIDBConnectionAndTZConnectionConnectTest;
begin
  FConnection.Connect;
  Assert.AreEqual(FConnection.IsConnected, FZConnection.Connected, 'FConnection.IsConnected <> FSQLConnection.Connected');
end;

procedure TFactoryZeosTests.FactoryZeosIDBConnectionAndTZConnectionDisconnectTest;
begin
  FConnection.Connect;
  FConnection.Disconnect;
  Assert.AreEqual(FConnection.IsConnected, FZConnection.Connected, 'FConnection.IsConnected <> FSQLConnection.Connected');
end;

procedure TFactoryZeosTests.FactoryZeosIDBConnectionAndTZConnectionInTransactionTest;
begin
  FConnection.Connect;
  FConnection.StartTransaction;
  Assert.AreEqual(FConnection.InTransaction, FZConnection.InTransaction, 'FConnection.InTransaction <> FSQLConnection.InTransaction');
end;

procedure TFactoryZeosTests.FactoryZeosIDBConnectionConnectTest;
begin
  FConnection.Connect;
  Assert.IsTrue(FConnection.IsConnected, 'FConnection.IsConnected = false');
end;


procedure TFactoryZeosTests.FactoryZeosIDBConnectionDisconnectTest;
begin
  FConnection.Connect;
  FConnection.Disconnect;
  Assert.IsFalse(FConnection.IsConnected, 'FConnection.IsConnected = true');
end;

initialization
  TDUnitX.RegisterTestFixture(TFactoryZeosTests);

end.

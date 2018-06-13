unit Tests.Factory.FireDAC;

interface
uses
  DUnitX.TestFramework,
  System.SysUtils,
  FireDAC.Comp.Client,
  FireDAC.Stan.Def,
  FireDAC.Stan.Intf,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.VCLUI.Wait,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Comp.UI,
  FireDAC.DApt,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.Comp.DataSet,
  ormbr.factory.interfaces,
  ormbr.factory.firedac,
  ormbr.dependency.interfaces,
  ormbr.dependency.injection.fdmemtable,
  ormbr.criteria,
  ormbr.model.master,
  ormbr.model.detail,
  ormbr.model.lookup,
  ormbr.model.client,
  ormbr.types.database,
  Helper.Dataset;

type

  [TestFixture]
  TFactoryFireDACTests = class(TObject)
  private
    FDConnection      : TFDConnection;
    FConnection       : IDBConnection;
    FIDBQuery         : IDBQuery;
    FModelClientDemo  : IContainerDataSet<Tclient>;
    FDataSetClientDemo: TFDMemTable;
    FRowCountTest     : Integer;

    const
      SQL_INSERT_TEST = ' INSERT INTO client (client_id, client_name) VALUES (%d, %s); ';
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure FactoryFireDACIDBConnectionConnectTest;
    [Test]
    procedure FactoryFireDACIDBConnectionDisconnectTest;
    [Test]
    procedure FactoryFireDACIDBConnectionAndFDConnectionConnectTest;
    [Test]
    procedure FactoryFireDACIDBConnectionAndFDConnectionDisconnectTest;
    [Test]
    procedure FactoryFireDACInTransactionTest;
    [Test]
    procedure FactoryFireDACIDBConnectionAndFDConnectionInTransactionTest;
    [Test]
    procedure FactoryFireDACCommitTest;
    [Test]
    procedure FactoryFireDACRollbackTest;
    [Test]
    procedure FactoryFireDACExecuteDirectTest;
  end;

implementation

procedure TFactoryFireDACTests.FactoryFireDACCommitTest;
var
  oKeyValue : Integer;
begin
  try
    FDataSetClientDemo := TFDMemTable.Create(nil);
    FModelClientDemo   := TContainerFDMemTable<Tclient>.Create(FConnection, FDataSetClientDemo);
    FModelClientDemo.DataSet.Open;

    oKeyValue     := FDataSetClientDemo.GetNextKeyValue('client_id') + 1;
    FRowCountTest := FDataSetClientDemo.RecordCount + 1;

    FConnection.StartTransaction;

    FIDBQuery := FConnection.CreateQuery;
    FIDBQuery.CommandText := Format(SQL_INSERT_TEST,[oKeyValue, QuotedStr('FireDAC CommitTest ' + oKeyValue.ToString)]);
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

procedure TFactoryFireDACTests.FactoryFireDACRollbackTest;
var
  oKeyValue : Integer;
begin
  try
    FDataSetClientDemo := TFDMemTable.Create(nil);
    FModelClientDemo   := TContainerFDMemTable<Tclient>.Create(FConnection, FDataSetClientDemo);
    FModelClientDemo.DataSet.Open;

    oKeyValue     := FDataSetClientDemo.GetNextKeyValue('client_id') + 1;
    FRowCountTest := FDataSetClientDemo.RecordCount + 1;

    FConnection.StartTransaction;

    FIDBQuery := FConnection.CreateQuery;
    FIDBQuery.CommandText := Format(SQL_INSERT_TEST,[oKeyValue, QuotedStr('FireDAC RollbackTest ' + oKeyValue.ToString)]);
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

procedure TFactoryFireDACTests.FactoryFireDACExecuteDirectTest;
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

procedure TFactoryFireDACTests.FactoryFireDACIDBConnectionAndFDConnectionConnectTest;
begin
  FConnection.Connect;
  Assert.AreEqual(FConnection.IsConnected, FDConnection.Connected, 'FConnection.IsConnected <> FSQLConnection.Connected');
end;

procedure TFactoryFireDACTests.FactoryFireDACIDBConnectionAndFDConnectionDisconnectTest;
begin
  FConnection.Connect;
  FConnection.Disconnect;
  Assert.AreEqual(FConnection.IsConnected, FDConnection.Connected, 'FConnection.IsConnected <> FSQLConnection.Connected');
end;

procedure TFactoryFireDACTests.FactoryFireDACIDBConnectionAndFDConnectionInTransactionTest;
begin
  FConnection.Connect;
  FConnection.StartTransaction;
  Assert.AreEqual(FConnection.InTransaction, FDConnection.InTransaction, 'FConnection.InTransaction <> FSQLConnection.InTransaction');
end;

procedure TFactoryFireDACTests.FactoryFireDACIDBConnectionConnectTest;
begin
  FConnection.Connect;
  Assert.IsTrue(FConnection.IsConnected, 'FConnection.IsConnected = false');
end;

procedure TFactoryFireDACTests.FactoryFireDACIDBConnectionDisconnectTest;
begin
  FConnection.Connect;
  FConnection.Disconnect;
  Assert.IsFalse(FConnection.IsConnected, 'FConnection.IsConnected = true');
end;

procedure TFactoryFireDACTests.FactoryFireDACInTransactionTest;
begin
  FConnection.Connect;
  FConnection.StartTransaction;
  Assert.IsTrue(FConnection.InTransaction, 'FConnection.InTransaction = false');
end;

procedure TFactoryFireDACTests.Setup;
begin
  FDConnection := TFDConnection.Create(nil);

  FDConnection.Params.DriverID := 'SQLite';
  FDConnection.Params.Database := '..\Demo\Data\Database\database.db3';
  FDConnection.LoginPrompt := False;
  FDConnection.TxOptions.Isolation := xiReadCommitted;
  FDConnection.TxOptions.AutoCommit := False;

  FConnection := TFactoryFireDAC.Create(FDConnection, dnSQLite);
end;

procedure TFactoryFireDACTests.TearDown;
begin
  if Assigned(FDConnection) then
    FreeAndNil(FDConnection);
end;

initialization
  TDUnitX.RegisterTestFixture(TFactoryFireDACTests);
end.

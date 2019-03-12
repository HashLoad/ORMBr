unit Tests.Driver.AbsoluteDB;

interface
uses
  DUnitX.TestFramework,

  System.SysUtils,
  Data.DB,

  ABSMain,

  ormbr.factory.interfaces;

type
  [TestFixture]
  TTestDriverConnection = class(TObject)
  strict private
    FConnection: TABSDatabase;
    FDBConnection: IDBConnection;
    FDBQuery: IDBQuery;
    FDBResultSet: IDBResultSet;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestConnect;
    [Test]
    procedure TestDisconnect;
    [Test]
    procedure TestExecuteDirect;
    [Test]
    procedure TestExecuteDirectParams;
    [Test]
    procedure TestExecuteScript;
    [Test]
    procedure TestAddScript;
    [Test]
    procedure TestExecuteScripts;
    [Test]
    procedure TestIsConnected;
    [Test]
    procedure TestInTransaction;
    [Test]
    procedure TestCreateQuery;
    [Test]
    procedure TestCreateResultSet;
    [Test]
    procedure TestExecuteSQL;
    [Test]
    procedure TestStartTransaction;
    [Test]
    procedure TestCommit;
    [Test]
    procedure TestRollback;
  end;

implementation

uses
  ormbr.factory.absolutedb,
  Tests.Consts;

{ TTestDriverConnection }

procedure TTestDriverConnection.TestCommit;
begin
  TestStartTransaction;

  FDBConnection.Commit;
  Assert.IsFalse(FDBConnection.InTransaction, 'FConnection.InTransaction = false');
end;

procedure TTestDriverConnection.TestExecuteDirect;
var
  LValue: String;
  LRandon: String;
begin
  LRandon := IntToStr( Random(9999) );

  FDBConnection.ExecuteDirect( Format(cSQLUPDATE, [QuotedStr(cDESCRIPTION + LRandon), '1']) );

  FDBQuery := FDBConnection.CreateQuery;
  FDBQuery.CommandText := Format(cSQLSELECT, ['1']);
  LValue := FDBQuery.ExecuteQuery.FieldByName('CLIENT_NAME').AsString;

  Assert.AreEqual(LValue, cDESCRIPTION + LRandon, LValue + ' <> ' + cDESCRIPTION + LRandon);
end;

procedure TTestDriverConnection.TestExecuteDirectParams;
var
  LParams: TParams;
  LRandon: String;
  LValue: String;
begin
  LRandon := IntToStr( Random(9999) );

  LParams := TParams.Create(nil);
  try
    with LParams.Add as TParam do
    begin
      Name := 'CLIENT_NAME';
      DataType := ftString;
      Value := cDESCRIPTION + LRandon;
      ParamType := ptInput;
    end;
    with LParams.Add as TParam do
    begin
      Name := 'CLIENT_ID';
      DataType := ftInteger;
      Value := 1;
      ParamType := ptInput;
    end;
    FDBConnection.ExecuteDirect(cSQLUPDATEPARAM, LParams);

    FDBResultSet := FDBConnection.CreateResultSet(Format(cSQLSELECT, ['1']));
    LValue := FDBResultSet.FieldByName('CLIENT_NAME').AsString;

    Assert.AreEqual(LValue, cDESCRIPTION + LRandon, LValue + ' <> ' + cDESCRIPTION + LRandon);
  finally
    LParams.Free;
  end;
end;

procedure TTestDriverConnection.TestExecuteScript;
begin
  Assert.Pass('');
end;

procedure TTestDriverConnection.TestExecuteScripts;
begin
  Assert.Pass('');
end;

procedure TTestDriverConnection.TestExecuteSQL;
begin
  FDBResultSet := FDBConnection.ExecuteSQL( Format(cSQLSELECT, ['1']) );

  Assert.IsTrue(FDBResultSet.RecordCount = 1, 'FDBResultSet.RecordCount = ' + IntToStr(FDBResultSet.RecordCount));
end;

procedure TTestDriverConnection.TestInTransaction;
begin
  FDBConnection.Connect;
  FDBConnection.StartTransaction;

  Assert.AreEqual(FDBConnection.InTransaction, FConnection.InTransaction, 'FConnection.InTransaction <> FFDConnection.InTransaction');

  FDBConnection.Rollback;
  FDBConnection.Disconnect;
end;

procedure TTestDriverConnection.TestIsConnected;
begin
  Assert.IsFalse(FDBConnection.IsConnected, 'FConnection.IsConnected = false');
end;

procedure TTestDriverConnection.TestRollback;
begin
  TestStartTransaction;

  FDBConnection.Rollback;
  Assert.IsFalse(FDBConnection.InTransaction, 'FConnection.InTransaction = false');
end;

procedure TTestDriverConnection.Setup;
begin
  FConnection := TABSDatabase.Create(nil);
  FConnection.DatabaseFileName := '.\database.abs';
  FConnection.DatabaseName := 'database.abs';

  FDBConnection := TFactoryAbsoluteDB.Create(FConnection, dnAbsoluteDB);
end;

procedure TTestDriverConnection.TestStartTransaction;
begin
  FDBConnection.StartTransaction;
  Assert.IsTrue(FDBConnection.InTransaction, 'FConnection.InTransaction = true');
end;

procedure TTestDriverConnection.TearDown;
begin
  if Assigned(FConnection) then
    FreeAndNil(FConnection);
end;

procedure TTestDriverConnection.TestAddScript;
begin
  Assert.Pass('');
end;

procedure TTestDriverConnection.TestConnect;
begin
  FDBConnection.Connect;
  Assert.IsTrue(FDBConnection.IsConnected, 'FConnection.IsConnected = true');
end;

procedure TTestDriverConnection.TestCreateQuery;
var
  LValue: String;
  LRandon: String;
begin
  LRandon := IntToStr( Random(9999) );

  FDBQuery := FDBConnection.CreateQuery;
  FDBQuery.CommandText := Format(cSQLUPDATE, [QuotedStr(cDESCRIPTION + LRandon), '1']);
  FDBQuery.ExecuteDirect;

  FDBQuery.CommandText := Format(cSQLSELECT, ['1']);
  LValue := FDBQuery.ExecuteQuery.FieldByName('CLIENT_NAME').AsString;

  Assert.AreEqual(LValue, cDESCRIPTION + LRandon, LValue + ' <> ' + cDESCRIPTION + LRandon);
end;

procedure TTestDriverConnection.TestCreateResultSet;
begin
  FDBResultSet := FDBConnection.CreateResultSet(Format(cSQLSELECT, ['1']));

  Assert.IsTrue(FDBResultSet.RecordCount = 1, 'FDBResultSet.RecordCount = ' + IntToStr(FDBResultSet.RecordCount));
end;

procedure TTestDriverConnection.TestDisconnect;
begin
  FDBConnection.Disconnect;
  Assert.IsFalse(FDBConnection.IsConnected, 'FConnection.IsConnected = false');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestDriverConnection);
end.

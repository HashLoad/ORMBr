unit SessionManagerTests;

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
  Helper.Dataset;

type

  [TestFixture]
  TSessionManagerTests = class(TObject)
  private
    FSQLConnection    : TSQLConnection;
    FConnection       : IDBConnection;
    FModelMasterDemo  : IContainerDataSet<Tmaster>;
    FDataSetMasterDemo: TClientDataSet;
    FRowCountTest     : Integer;
    FCurrentID        : Integer;

    const
      EXPECTED_SQL_INSERT = 'INSERT INTO master (master_id, description, registerdate, updatedate, client_id) ' +
                            'VALUES (%d, ''Master Demo Test %d'', ''%s'', ''%s'', 1)';

      EXPECTED_SQL_DELETE = 'DELETE FROM master WHERE master_id=%d';

    function GetExpectedSQLInsert: string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure GetSQLInsertTest;
    [Test]
    procedure GetSQLDeleteTest;
  end;

implementation

function TSessionManagerTests.GetExpectedSQLInsert: string;
begin
  Result := Format(EXPECTED_SQL_INSERT, [FRowCountTest, FRowCountTest, FormatDateTime('YYYY-MM-DD', Date), FormatDateTime('YYYY-MM-DD', Date)]);
end;

procedure TSessionManagerTests.GetSQLDeleteTest;
begin
  FDataSetMasterDemo  := TClientDataSet.Create(nil);
  FModelMasterDemo    := TContainerClientDataSet<Tmaster>.Create(FConnection, FDataSetMasterDemo);

  FModelMasterDemo.DataSet.Open;
  FDataSetMasterDemo.Open;
  FDataSetMasterDemo.Last;
  FCurrentID := FDataSetMasterDemo.FieldByName('master_Id').AsInteger;

  FModelMasterDemo.DataSet.Close;
  FModelMasterDemo.DataSet.Open(FCurrentID);
  FModelMasterDemo.DataSet.Find;
  FModelMasterDemo.DataSet.Session.Delete(FModelMasterDemo.DataSet.CurrentRecord);
  FModelMasterDemo.DataSet.Session.Manager.GetSQLCommand;

  Assert.AreEqual(Format(EXPECTED_SQL_DELETE, [FCurrentID]), FModelMasterDemo.DataSet.Session.Manager.GetSQLCommand, 'EXPECTED_SQL_DELETE <> GetSQLCommand');
end;

procedure TSessionManagerTests.GetSQLInsertTest;
begin
  FModelMasterDemo.DataSet.Open;
  FRowCountTest := FDataSetMasterDemo.RecordCount + 1;

  FModelMasterDemo.DataSet.Append;
  FModelMasterDemo.DataSet.CurrentRecord.master_Id    := FRowCountTest;
  FModelMasterDemo.DataSet.CurrentRecord.description  := 'Master Demo Test '+ IntToStr(FRowCountTest);
  FModelMasterDemo.DataSet.CurrentRecord.updatedate   := StrToDate(FormatDateTime('DD/MM/YYYY', Date));
  FModelMasterDemo.DataSet.CurrentRecord.registerdate := StrToDate(FormatDateTime('DD/MM/YYYY', Date));
  FModelMasterDemo.DataSet.CurrentRecord.client_id    := 1;
  FModelMasterDemo.DataSet.Post;
  FModelMasterDemo.DataSet.ApplyUpdates(0);

  Assert.AreEqual(GetExpectedSQLInsert, FModelMasterDemo.DataSet.Session.Manager.GetSQLCommand, 'GetExpectedSQLInsert <> GetSQLCommand');
end;

procedure TSessionManagerTests.Setup;
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

  FDataSetMasterDemo  := TClientDataSet.Create(nil);
  FModelMasterDemo    := TContainerClientDataSet<Tmaster>.Create(FConnection, FDataSetMasterDemo);
end;

procedure TSessionManagerTests.TearDown;
begin
  if Assigned(FDataSetMasterDemo) then
    FreeAndNil(FDataSetMasterDemo);

  if Assigned(FSQLConnection) then
    FreeAndNil(FSQLConnection);
end;


initialization
  TDUnitX.RegisterTestFixture(TSessionManagerTests);
end.

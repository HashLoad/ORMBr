unit Tests.Session.Manager;

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
  TSessionManagerTests = class(TObject)
  private
    FSQLConnection    : TSQLConnection;
    FConnection       : IDBConnection;
    FModelMasterDemo  : IContainerDataSet<Tmaster>;
    FDataSetMasterDemo: TClientDataSet;
    FRowCountTest     : Integer;
    FCurrentID        : Integer;
    FNewDescription   : string;

    const
      EXPECTED_SQL_INSERT = 'INSERT INTO master (master_id, description, registerdate, updatedate, client_id) ' +
                            'VALUES (%d, ''Master Demo Test %d'', ''%s'', ''%s'', 1)';
      EXPECTED_SQL_DELETE = 'DELETE FROM master WHERE master_id=%d';
      EXPECTED_SQL_UPDATE = 'UPDATE master SET description = ''%s'', registerdate = ''%s'', updatedate = ''%s'', client_id = 1 WHERE master_id=%d';

    function GetExpectedSQLInsert: string;
    function GetExpectedSQLUpdate: string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure GetSQLInsertTest;
    [Test]
    procedure GetSQLDeleteTest;
    [Test]
    procedure GetSQLUpdateTest;
  end;

implementation

function TSessionManagerTests.GetExpectedSQLInsert: string;
begin
  Result := Format(EXPECTED_SQL_INSERT, [FRowCountTest, FRowCountTest, FormatDateTime('YYYY-MM-DD', Date), FormatDateTime('YYYY-MM-DD', Date)]);
end;

function TSessionManagerTests.GetExpectedSQLUpdate: string;
begin
  Result := Format(EXPECTED_SQL_UPDATE, [FNewDescription, FormatDateTime('YYYY-MM-DD', Date), FormatDateTime('YYYY-MM-DD', Date), FCurrentID]);
end;

procedure TSessionManagerTests.GetSQLDeleteTest;
var
s: string;
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
  FModelMasterDemo.DataSet.Session.Delete(FModelMasterDemo.DataSet.Current);

  Assert.AreEqual(Format(EXPECTED_SQL_DELETE, [FCurrentID]), FModelMasterDemo.DataSet.Session.Manager.GetDMLCommand, 'EXPECTED_SQL_DELETE <> GetSQLCommand');
end;

procedure TSessionManagerTests.GetSQLInsertTest;
begin
  FModelMasterDemo.DataSet.Open;
  FRowCountTest := FDataSetMasterDemo.RecordCount + 1;

  FModelMasterDemo.DataSet.Append;
  FModelMasterDemo.DataSet.Current.master_Id    := FRowCountTest;
  FModelMasterDemo.DataSet.Current.description  := 'Master Demo Test '+ IntToStr(FRowCountTest);
  FModelMasterDemo.DataSet.Current.updatedate   := Date;
  FModelMasterDemo.DataSet.Current.registerdate := Date;
  FModelMasterDemo.DataSet.Current.client_id    := 1;
  FModelMasterDemo.DataSet.Post;
  FModelMasterDemo.DataSet.ApplyUpdates(0);

  Assert.AreEqual(GetExpectedSQLInsert, FModelMasterDemo.DataSet.Session.Manager.GetDMLCommand, 'GetExpectedSQLInsert <> GetSQLCommand');
end;

procedure TSessionManagerTests.GetSQLUpdateTest;
begin
  FDataSetMasterDemo  := TClientDataSet.Create(nil);
  FModelMasterDemo    := TContainerClientDataSet<Tmaster>.Create(FConnection, FDataSetMasterDemo);

  FModelMasterDemo.DataSet.Open;
  FDataSetMasterDemo.Open;
  FDataSetMasterDemo.Last;
  FCurrentID := FDataSetMasterDemo.FieldByName('master_Id').AsInteger;
  FNewDescription := 'Master Demo Test '+ IntToStr(FCurrentID);

  FModelMasterDemo.DataSet.Close;
  FModelMasterDemo.DataSet.Open(FCurrentID);
  FModelMasterDemo.DataSet.Find;

  FModelMasterDemo.DataSet.Edit;
  FModelMasterDemo.DataSet.Current.master_Id    := FCurrentID;
  FModelMasterDemo.DataSet.Current.description  := FNewDescription;
  FModelMasterDemo.DataSet.Current.updatedate   := StrToDate(FormatDateTime('DD/MM/YYYY', Date));
  FModelMasterDemo.DataSet.Current.registerdate := StrToDate(FormatDateTime('DD/MM/YYYY', Date));
  FModelMasterDemo.DataSet.Current.client_id    := 1;
  FModelMasterDemo.DataSet.Post;
  FModelMasterDemo.DataSet.ApplyUpdates(0);
  FModelMasterDemo.DataSet.Session.Manager.GetDMLCommand;

  Assert.AreEqual(GetExpectedSQLUpdate, FModelMasterDemo.DataSet.Session.Manager.GetDMLCommand, 'GetExpectedSQLUpdate <> GetSQLCommand');
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

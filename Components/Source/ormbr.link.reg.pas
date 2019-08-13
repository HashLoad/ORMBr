unit ormbr.link.reg;

interface

uses
  Classes,
  DesignIntf,
  DesignEditors,
  ormbr.driver.link.firebird,
  ormbr.driver.link.interbase,
  ormbr.driver.link.mongodb,
  ormbr.driver.link.oracle,
  ormbr.driver.link.mysql,
  ormbr.driver.link.mssql,
  ormbr.driver.link.postgresql,
  ormbr.driver.link.sqldirect,
  ormbr.driver.link.sqlite;

type
  TORMBrDriverEditorFirebird = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TORMBrDriverEditorInterbase = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TORMBrDriverEditorMSSQL = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TORMBrDriverEditorMySQL = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TORMBrDriverEditorOracle = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TORMBrDriverEditorMongoDB = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TORMBrDriverEditorPostgreSQL = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TORMBrDriverEditorSQLite = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TORMBrDriverEditorSQLDirect = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('ORMBr-Links', [TORMBrDriverLinkFirebird,
                                     TORMBrDriverLinkInterbase,
                                     TORMBrDriverLinkMSSQL,
                                     TORMBrDriverLinkMYSQL,
                                     TORMBrDriverLinkOracle,
                                     TORMBrDriverLinkMongoDB,
                                     TORMBrDriverLinkPostgreSQL,
                                     TORMBrDriverLinkSQLite,
                                     TORMBrDriverLinkSQLDirect
                                    ]);
  RegisterSelectionEditor(TORMBrDriverLinkFirebird, TORMBrDriverEditorFirebird);
  RegisterSelectionEditor(TORMBrDriverLinkInterbase, TORMBrDriverEditorInterbase);
  RegisterSelectionEditor(TORMBrDriverLinkMSSQL, TORMBrDriverEditorMSSQL);
  RegisterSelectionEditor(TORMBrDriverLinkMYSQL, TORMBrDriverEditorMySQL);
  RegisterSelectionEditor(TORMBrDriverLinkOracle, TORMBrDriverEditorOracle);
  RegisterSelectionEditor(TORMBrDriverLinkMongoDB, TORMBrDriverEditorMongoDB);
  RegisterSelectionEditor(TORMBrDriverLinkPostgreSQL, TORMBrDriverEditorPostgreSQL);
  RegisterSelectionEditor(TORMBrDriverLinkSQLite, TORMBrDriverEditorSQLite);
  RegisterSelectionEditor(TORMBrDriverLinkSQLDirect, TORMBrDriverEditorSQLDirect);
end;

{ TORMBrDriverEditorFirebird }

procedure TORMBrDriverEditorFirebird.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('ormbr.dml.generator.firebird');
end;

{ TORMBrDriverEditorMSSQL }

procedure TORMBrDriverEditorMSSQL.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('ormbr.dml.generator.mssql');
end;

{ TORMBrDriverEditorMongoDB }

procedure TORMBrDriverEditorMongoDB.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('ormbr.dml.generator.mongodb');
end;

{ TORMBrDriverEditorOracle }

procedure TORMBrDriverEditorOracle.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('ormbr.dml.generator.oracle');
end;

{ TORMBrDriverEditorMySQL }

procedure TORMBrDriverEditorMySQL.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('ormbr.dml.generator.mysql');
end;

{ TORMBrDriverEditorPostgreSQL }

procedure TORMBrDriverEditorPostgreSQL.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('ormbr.dml.generator.postgresql');
end;

{ TORMBrDriverEditorInterbase }

procedure TORMBrDriverEditorInterbase.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('ormbr.dml.generator.interbase');
end;

{ TORMBrDriverEditorSQLite }

procedure TORMBrDriverEditorSQLite.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('ormbr.dml.generator.sqlite');
end;

{ TORMBrDriverEditorSQLDirect }

procedure TORMBrDriverEditorSQLDirect.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('ormbr.dml.generator.sqldirect');
end;

end.

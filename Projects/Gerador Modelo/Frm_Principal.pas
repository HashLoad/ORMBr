  unit Frm_Principal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, WideStrings, Buttons, StdCtrls, DB, ExtCtrls,
  ComCtrls, FMTBcd, MidasLib, DBClient, Menus, DBCtrls,
  Mask, AnsiStrings,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB, FireDAC.Phys.FBDef,
  FireDAC.VCLUI.Wait, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteDef, FireDAC.Phys.MySQLDef, FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.IBBase, FireDAC.Phys.ODBCBase, FireDAC.Phys.MSSQL,
  FireDAC.Phys.MySQL, FireDAC.Phys.SQLite, FireDAC.Comp.UI,
  Generics.Collections, Vcl.Grids, Vcl.DBGrids, Vcl.WinXCtrls, Vcl.Imaging.pngimage,
  Vcl.DBCGrids,StrUtils, FireDAC.Phys.OracleDef, FireDAC.Phys.DB2Def,
  FireDAC.Phys.IBDef, FireDAC.Phys.IB, FireDAC.Phys.DB2, FireDAC.Phys.Oracle,
  FireDAC.Phys.PGDef, FireDAC.Phys.PG;

type
  TFrmPrincipal = class(TForm)
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    Entidade_: TFDTable;
    FDPhysSQLiteDriverLink2: TFDPhysSQLiteDriverLink;
    Metadata: TFDMetaInfoQuery;
    Fields: TFDMetaInfoQuery;
    Entidade: TFDQuery;
    pnDisplayCode: TPanel;
    Panel5: TPanel;
    PageControl1: TPageControl;
    tabModel: TTabSheet;
    Panel3: TPanel;
    Splitter1: TSplitter;
    lstTabelas: TListBox;
    Splitter3: TSplitter;
    Panel4: TPanel;
    Panel6: TPanel;
    lstCampos: TListBox;
    Panel7: TPanel;
    CDS_CNN: TClientDataSet;
    CDS_CNNCNN_Type: TStringField;
    CDS_CNNCNN_Name: TStringField;
    CDS_CNNCNN_Server: TStringField;
    CDS_CNNCNN_Database: TStringField;
    CDS_CNNCNN_Schema: TStringField;
    CDS_CNNCNN_UserName: TStringField;
    CDS_CNNCNN_Password: TStringField;
    DTS_CNN: TDataSource;
    pnCONN: TPanel;
    Panel9: TPanel;
    pnCONN_NAV: TPanel;
    DBNavigator1: TDBNavigator;
    Panel10: TPanel;
    DBText3: TDBText;
    DBRichEdit1: TDBRichEdit;
    Combo_Connection: TComboBox;
    btnConectar: TBitBtn;
    pnConfig: TPanel;
    btnReverseAll: TButton;
    edtProjeto: TEdit;
    Label1: TLabel;
    Panel2: TPanel;
    edtPath: TEdit;
    Label2: TLabel;
    memModel: TMemo;
    FDConn: TFDConnection;
    CDS_CNNCNN_Port: TIntegerField;
    FDPhysOracleDriverLink1: TFDPhysOracleDriverLink;
    FDPhysDB2DriverLink1: TFDPhysDB2DriverLink;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDPhysPgDriverLink1: TFDPhysPgDriverLink;
    Button1: TButton;
    checkLowerCase: TCheckBox;
    checkLazy: TCheckBox;
    procedure lstTabelasClick(Sender: TObject);
    procedure btnReverseAllClick(Sender: TObject);
    procedure memoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CDS_CNNAfterInsert(DataSet: TDataSet);
    procedure CDS_CNNNewRecord(DataSet: TDataSet);
    procedure Combo_ConnectionChange(Sender: TObject);
    procedure CDS_CNNBeforePost(DataSet: TDataSet);
    procedure CDS_CNNAfterPost(DataSet: TDataSet);
    procedure CDS_CNNAfterDelete(DataSet: TDataSet);
    procedure btnConectarClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FAppPathName : String;

    _PrimaryKey: TStringList;
    _Id: TStringList;
    _Indexes: TStringList;
    _Checks: TStringList;
    _FieldsProperty: TStringList;
    _Propertys: TStringList;
    _Declaration: TStringList;
    _Implementation: TStringList;
    _FieldsRelations : TStringList;
    _PropertysRelations : TStringList;
    _UsesRelations : TStringList;
    _ConstructorDeclaration : TStringList;
    _ConstructorImplementation : TStringList;
    _DestructorDeclaration : TStringList;
    _DestructorImplementation : TStringList;
    _ForeignKeys: TDictionary<String,String>;
    _LazyLoadImplementation: TStringList;

    procedure SaveConnection;
    procedure LoadConnection;

    //Get Parametros por Tabela
    procedure GetTableParam(Index : Integer);
    //Get Parametros por Property (Campos)
    procedure LoadPropertys(Index : Integer);
    //Corpo das Class
    procedure CreateBodyClassUnit(index: Integer);
    //Gera Class
    procedure GenerateClassUnit;
    procedure Conectar(Driver: string; Conn: TFDConnection;
      Server, Database, User, Pass: string; Port: Integer = 0);
    procedure GetPK(index: Integer);
    function DriverCatalogExists: Boolean;
  public
    { Public declarations }
  end;

var
  FrmPrincipal: TFrmPrincipal;

implementation

{$R *.dfm}

uses Frm_Connection;

procedure TFrmPrincipal.CDS_CNNAfterDelete(DataSet: TDataSet);
begin
   SaveConnection;
   LoadConnection;
end;

procedure TFrmPrincipal.CDS_CNNAfterInsert(DataSet: TDataSet);
begin
  FrmConnection       := TFrmConnection.Create(Self);
  FrmConnection.Left  := pnCONN.Left;
  FrmConnection.Width := pnCONN.Width;
  FrmConnection.Top   := (pnCONN.Top + pnCONN.Height + 28);
  FrmConnection.Show;
end;

procedure TFrmPrincipal.CDS_CNNAfterPost(DataSet: TDataSet);
begin
   SaveConnection;
   LoadConnection;
end;

procedure TFrmPrincipal.CDS_CNNBeforePost(DataSet: TDataSet);
begin
  CDS_CNN.FieldByName('CNN_NAME').AsString := '[ '+UpperCase(CDS_CNN.FieldByName('CNN_TYPE').AsString)+' ] '+ CDS_CNN.FieldByName('CNN_NAME').AsString;
end;

procedure TFrmPrincipal.CDS_CNNNewRecord(DataSet: TDataSet);
begin
   CDS_CNN.FieldByName('CNN_TYPE').AsString := 'MSSQL';
   CDS_CNN.FieldByName('CNN_SERVER').AsString := 'LOCALHOST';
   CDS_CNN.FieldByName('CNN_NAME').AsString := 'Conex�o Local ';
end;

procedure TFrmPrincipal.Combo_ConnectionChange(Sender: TObject);
begin
   CDS_CNN.Locate('CNN_NAME',Combo_Connection.Text,[]);
end;

procedure TFrmPrincipal.Conectar(Driver: string; Conn: TFDConnection;
  Server, Database, User, Pass: string; Port: Integer = 0);
const
  DBOracle = '(DESCRIPTION=(ADDRESS=(PROTOCOL=TCP)(HOST=%s)(PORT=%s))(CONNECT_DATA=(SERVICE_NAME=XE)))';
begin
  Conn.Connected := false;
  if (Driver = 'MSSQL') then
  begin
    Conn.Params.Clear;
    Conn.DriverName                      := 'MSSQL';
    Conn.Params.DriverID                 := 'MSSQL';
    Conn.Params.Values['Server']         := Server;
    Conn.Params.Values['DataBase']       := Database;
    if (Length(Trim(User)) = 0)and(Length(Trim(Pass)) = 0) then
    begin
      Conn.Params.Values['OSAuthent']    := 'Yes';
      Conn.Params.Values['User_Name']    := '';//User;
      Conn.Params.Values['Password']     := '';//Pass;
    end
    else
    begin
      Conn.Params.Values['OSAuthent']    := 'No';
      Conn.Params.Values['User_Name']    := User;
      Conn.Params.Values['Password']     := Pass;
    end;
    Conn.Params.Values['MetaDefSchema']  := 'dbo';
    Conn.Params.Values['MetaDefCatalog'] := Database;
    Conn.Params.Values['DriverID']       := 'MSSQL';
  end
  else
  if (Driver = 'Firebird') then
  begin
     Conn.Params.Clear;
     Conn.DriverName                     := 'FB';
     Conn.Params.DriverID                := 'FB';
     Conn.Params.Values['DriverID']      := 'FB';
     Conn.Params.Values['Server']        := Server;
     if Port > 0 then
        Conn.Params.Values['Port']       := IntToStr(Port);
     Conn.Params.Values['DataBase']      := Database;
     Conn.Params.Values['User_Name']     := User;
     Conn.Params.Values['Password']      := Pass;
  end
  else
  if (Driver = 'Interbase') then
  begin
     Conn.Params.Clear;
     Conn.DriverName                     := 'IB';
     Conn.Params.DriverID                := 'IB';
     Conn.Params.Values['DriverID']      := 'IB';
     Conn.Params.Values['Server']        := Server;
     if Port > 0 then
        Conn.Params.Values['Port']       := IntToStr(Port);
     Conn.Params.Values['DataBase']      := Database;
     Conn.Params.Values['User_Name']     := User;
     Conn.Params.Values['Password']      := Pass;
  end
  else
  if ( Driver = 'Oracle') then
  begin
     Conn.Params.Clear;
     Conn.DriverName                     := 'Ora';
     Conn.Params.DriverID                := 'Ora';
     Conn.Params.Values['DataBase']      := Format(DBOracle, [Server, IntToStr(Port)]);
     Conn.Params.Values['User_Name']     := User;
     Conn.Params.Values['Password']      := Pass;
  end
  else
  if ( Driver = 'MySQL') then
  begin
     Conn.Params.Clear;
     Conn.DriverName                     := 'MySQL';
     Conn.Params.DriverID                := 'MySQL';
     Conn.Params.Values['Server']        := Server;
     if Port > 0 then
        Conn.Params.Values['Port']       := IntToStr(Port);
     Conn.Params.Values['DataBase']      := Database;
     Conn.Params.Values['User_Name']     := User;
     Conn.Params.Values['Password']      := Pass;
  end
  else
  if ( Driver = 'SQLite') then
  begin
     Conn.DriverName                     := 'SQLite';
     Conn.Params.Clear;
     Conn.Params.DriverID                := 'SQLite';
     Conn.Params.Values['HostName']      := '';
     Conn.Params.Values['DataBase']      := Database;
     Conn.Params.Values['User_Name']     := '';
     Conn.Params.Values['Password']      := '';
  end
  else
  if ( Driver = 'PostgreSQL') then
  begin
     Conn.DriverName                     := 'PG';
     Conn.Params.Clear;
     Conn.Params.DriverID                := 'PG';
     Conn.Params.Values['Server']        := Server;
     if Port > 0 then
        Conn.Params.Values['Port']       := IntToStr(Port);
     Conn.Params.Values['DataBase']      := Database;
     Conn.Params.Values['User_Name']     := User;
     Conn.Params.Values['Password']      := Pass;
  end;
  Conn.Connected                         := true;
  Metadata.Connection                    := Conn;
end;

procedure TFrmPrincipal.LoadConnection;
begin
 if FileExists(ExtractFilePath(ParamStr(0))+'Connection.xml') then
   begin
      CDS_CNN.Close;
      CDS_CNN.CreateDataSet;
      CDS_CNN.LoadFromFile(FAppPathName+'Connection.xml');
      CDS_CNN.Open;
      CDS_CNN.First;

      Combo_Connection.Clear;
      while not CDS_CNN.Eof do
      begin
        Combo_Connection.Items.Add(CDS_CNN.FieldByName('CNN_Name').AsString);
        CDS_CNN.Next;
      end;
      Combo_Connection.ItemIndex := 0;
      Combo_ConnectionChange(Self);
   end
   else
   begin
    CDS_CNN.CreateDataSet;
   end;
end;

procedure TFrmPrincipal.SaveConnection;
begin
  CDS_CNN.SaveToFile(ExtractFilePath(ParamStr(0))+'Connection.xml', dfXML);
end;

{ Acrescentar mais drivers onde no --> lstTabelas <-- � listado o NOME_DO_BANCO.TABELA }
function TFrmPrincipal.DriverCatalogExists(): Boolean;
begin
  Result := AnsiMatchStr(UpperCase(FDConn.DriverName), ['MYSQL']);
end;

procedure TFrmPrincipal.lstTabelasClick(Sender: TObject);
begin
  Metadata.Connection.GetFieldNames('', '', lstTabelas.Items.Strings[lstTabelas.itemindex], '', lstCampos.items);
end;

procedure TFrmPrincipal.memoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = Ord('A')) then
     memModel.SelectAll;
end;

procedure TFrmPrincipal.btnConectarClick(Sender: TObject);
begin
  Conectar(CDS_CNN.FieldByName('CNN_TYPE').AsString,
           FDConn,
           CDS_CNN.FieldByName('CNN_SERVER').AsString,
           CDS_CNN.FieldByName('CNN_DATABASE').AsString,
           CDS_CNN.FieldByName('CNN_USERNAME').AsString,
           CDS_CNN.FieldByName('CNN_PASSWORD').AsString,
           CDS_CNN.FieldByName('CNN_PORT').AsInteger);

  if not FDConn.Connected then
    exit;

  if DriverCatalogExists() then
    with TFDMetaInfoQuery.Create(Self) do
      try
        Connection   := FDConn;
        ObjectScopes := [osMy];
        TableKinds   := [tkTable, tkView];
        CatalogName  := CDS_CNN.FieldByName('CNN_DATABASE').AsString;
        Active       := True;
        First;

        if not isEmpty then
        begin
          lstTabelas.Items.Clear;

          repeat
            Application.ProcessMessages;

            lstTabelas.Items.Add( FieldByName('TABLE_NAME').AsString );

            Next;
          until Eof;
        end;

      finally
        Free;
      end
  else
    Metadata.Connection.GetTableNames( '', '', '', lstTabelas.Items, [osMy], [tkTable, tkView] );

  if lstTabelas.itemindex > -1 then
     if not Trim( lstTabelas.Items.Strings[lstTabelas.itemindex] ).isEmpty then
        Metadata.Connection.GetFieldNames('', '', lstTabelas.Items.Strings[lstTabelas.itemindex], '', lstCampos.items);


end;

procedure TFrmPrincipal.btnReverseAllClick(Sender: TObject);
begin
   GenerateClassUnit;
end;


procedure TFrmPrincipal.Button1Click(Sender: TObject);
var
  OpenDialog: TFileOpenDialog;
begin
  OpenDialog := TFileOpenDialog.Create(Self);
  try
    OpenDialog.Options := OpenDialog.Options + [fdoPickFolders];
    if not OpenDialog.Execute then
      Abort;
    edtPath.Text := OpenDialog.FileName;
  finally
    OpenDialog.Free;
  end;
end;

procedure TFrmPrincipal.GenerateClassUnit;
var
intFor: Integer;
begin
  if lstTabelas.itemIndex > -1 then
  begin
     for intFor := 0 to lstTabelas.Count -1 do
     begin
       if lstTabelas.Selected[intFor] then
       begin
          Metadata.Connection.GetFieldNames('','',lstTabelas.Items.Strings[intFor],'',lstCampos.items);
          try
            //Cria  Variaveis de Controle
            _FieldsProperty            := TStringList.create;
            _Checks                    := TStringList.create;
            _Indexes                   := TStringList.create;
            _Propertys                 := TStringList.create;
            _PrimaryKey                := TStringList.create;
            _Id                        := TStringList.create;
            _FieldsRelations           := TStringList.create;
            _PropertysRelations        := TStringList.create;
            _ConstructorDeclaration    := TStringList.create;
            _ConstructorImplementation := TStringList.create;
            _DestructorDeclaration     := TStringList.create;
            _DestructorImplementation  := TStringList.create;
            _Declaration               := TStringList.create;
            _Implementation            := TStringList.create;
            _ForeignKeys               := TDictionary<String,String>.Create;
            _UsesRelations             := TStringList.Create;
            _LazyLoadImplementation    := TStringList.Create;
            //
            //Monta o Corpo da Unit
            GetTableParam(intFor);
            CreateBodyClassUnit(intFor);

            if not DirectoryExists(edtPath.Text) then
              CreateDir(edtPath.Text);
            memModel.Lines.SaveToFile(edtPath.Text + '\' + edtProjeto.Text + lstTabelas.Items.Strings[intFor]+'.pas');
          finally
            _FieldsProperty.Free;
            _Checks.Free;
            _PrimaryKey.Free;
            _Id.Free;
            _Indexes.Free;
            _FieldsRelations.Free;
            _Propertys.Free;
            _PropertysRelations.Free;
            _UsesRelations.Free;
            _ConstructorDeclaration.Free;
            _ConstructorImplementation.Free;
            _DestructorDeclaration.Free;
            _DestructorImplementation.Free;
            _Declaration.Free;
            _Implementation.Free;
            _ForeignKeys.Free;
            _LazyLoadImplementation.Free;
          end;
       end;
       Application.ProcessMessages;
     end;
  end;
end;



procedure TFrmPrincipal.GetPK(index: Integer);
begin
   with Metadata do
   begin
     Close;
     IndexFieldNames := '';
     BaseObjectName := lstTabelas.Items.Strings[index];
     MetaInfoKind := mkPrimaryKeyFields;
     Open;
   end;
end;

procedure TFrmPrincipal.GetTableParam(Index : Integer);
begin

   //FormatName(lstTabelas.Items.Strings[index])

   // Indexe
   //-------------------------------------------------------------------
//   _Indexes.Add('  { [Indexe('+QuotedStr('IDX_FirstName')+','+QuotedStr('FirstName') +', NoSort, True,'+QuotedStr('Indexe por nome')+')] }');

   //checks
//   _Checks.Add('  { [Check('+QuotedStr('CHK_Age')+', '+QuotedStr('Age >= 0')+')]   // Exemplo de uso do Check }');


end;

procedure  TFrmPrincipal.LoadPropertys(Index : Integer);

  function GetFieldType(pTipo :String): String;
  begin
    if pTipo = 'String'    then Result := 'ftString';
    if pTipo = 'Double'    then Result := 'ftBCD';
    if pTipo = 'Integer'   then Result := 'ftInteger';
    if pTipo = 'TTime'     then Result := 'ftTime';
    if pTipo = 'TDateTime' then Result := 'ftDateTime';
    if pTipo = 'Boolean'   then Result := 'ftBoolean';
    if pTipo = 'Currency'  then Result := 'ftCurrency';
    if pTipo = 'TBlob'     then Result := 'ftBlob';
  end;

  //Cria o Corpo das Propriedades e Fields
  procedure CreateBodyProperty(_ListIndex: Integer; _Campo, _Tipo, _ReadWrite, _Align, _Default: string; _Index: Integer; _Size: Integer = 0; _Precision: Integer = 0; _Scale: Integer = 0; _Mask: string = '''''');
  var
    NullableTipo: string;
    sKey: TPair<String,String>;
    iPos: Integer;
  begin
     NullableTipo := _Tipo;

     if not Entidade.FieldByName(_Campo).Required then
       if _Tipo <> 'TBlob' then
          NullableTipo := 'Nullable<' + NullableTipo + '>';

     // Fields
     _FieldsProperty.Add('    F' + _Campo + ': ' + NullableTipo + ';');

     // PrimaryKey()
     // [PrimaryKey('Id', NotInc, NoSort, False, 'Chave prim�ria')]
     //-------------------------------------------------------------------
     GetPK(_ListIndex);
     if Metadata.Locate('COLUMN_NAME',_Campo,[loCaseInsensitive]) then
      _PrimaryKey.Add('  [PrimaryKey('+QuotedStr(_Campo)+', NotInc, NoSort, False, '+QuotedStr('Chave prim�ria')+')]');

     // Required
     if Entidade.FieldByName(_Campo).Required then
      _Propertys.Add('    [Restrictions([NotNull])]');

     if _Size > 0 then
      _Propertys.Add('    [Column(' + QuotedStr(_Campo) + ', ' + GetFieldType(_Tipo) + ', ' + IntToStr(_Size) + ')]')
     else
     if (_Precision > 0) and (_Scale > 0) then
      _Propertys.Add('    [Column(' + QuotedStr(_Campo) + ', ' + GetFieldType(_Tipo) + ', ' + IntToStr(_Precision) + ', ' + IntToStr(_Scale) + ')]')
     else
      _Propertys.Add('    [Column(' + QuotedStr(_Campo) + ', ' + GetFieldType(_Tipo) + ')]');

     for sKey in _ForeignKeys.ToArray do
     begin
       iPos := Pos('$', sKey.Key);
       if _Campo = Copy(sKey.Key, 1, iPos -1) then
         _Propertys.Add(sKey.Value);
     end;

     _Propertys.Add('    [Dictionary(''' + _Campo + ''', ''Mensagem de valida��o'', ' + _Default + ', '''', ' + _Mask + ', ' + _Align +')]');
     _Propertys.Add('    property ' + IfThen(checkLowerCase.Checked, LowerCase(_Campo), _Campo) + ': ' + NullableTipo +
                         {' index ' + IntToStr(_Index) +} _ReadWrite);
  end;

  function GetRule(_Index: Integer): string;
  begin
    if _Index = 0 then Result := 'None'
    else
    if _Index = 1 then Result := 'Cascade'
    else
    if _Index = 2 then Result := 'SetNull'
    else
    if _Index = 3 then Result := 'SetDefault';
  end;

var
  I: Integer;
  ReadWrite, Campo:string;
  L, P, S: Integer;
  sUses: string;
  FK: Integer;
begin
  /// ForeignKeys
  Metadata.Close;
  Metadata.MetaInfoKind    := mkForeignKeys;
  Metadata.ObjectName      := lstTabelas.Items[index];
  Metadata.IndexFieldNames := 'PKEY_TABLE_NAME';
  Metadata.Open;

  //Existe ForeignKeys - Instancia Constructor e cria as Classes Relacionadas
  if Metadata.RecordCount > 0 then
  begin
     _ConstructorDeclaration.Add('    constructor Create;');
     _DestructorDeclaration.Add('    destructor Destroy; override;');

     _ConstructorImplementation.Add('');
     _ConstructorImplementation.Add('constructor T' +lstTabelas.Items.Strings[index] + '.Create;');
     _ConstructorImplementation.Add('begin');

     _DestructorImplementation.Add('');
     _DestructorImplementation.Add('destructor T' +lstTabelas.Items.Strings[index] + '.Destroy;');
     _DestructorImplementation.Add('begin');
  end;
  FK := -1;
  while not Metadata.Eof do
  begin
     Fields.Close;
     Fields.MetaInfoKind   := mkForeignKeyFields;
     Fields.BaseObjectName := Metadata.FieldByName('TABLE_NAME').AsString;
     Fields.ObjectName     := Metadata.FieldByName('FKEY_NAME').AsString;
     Fields.Open;

     //Criar Rela��o de Units Relacionadas
     sUses := '  ' + edtProjeto.Text + LowerCase(Metadata.FieldByName('PKEY_TABLE_NAME').AsString);
     if _UsesRelations.IndexOf(sUses) = -1 then
     _UsesRelations.Add(sUses);

     _FieldsRelations.Add('    F' + Metadata.FieldByName('PKEY_TABLE_NAME').AsString + '_' + IntToStr(_FieldsRelations.Count) + ': ' + Format('%s T' + Metadata.FieldByName('PKEY_TABLE_NAME').AsString + ' %s ;', [IfThen(checkLazy.Checked, 'Lazy<', EmptyStr), IfThen(checkLazy.Checked, '>', EmptyStr)]));

      // Monta o ReadWrite
     if checkLazy.Checked then
       ReadWrite := ' read get' + Metadata.FieldByName('PKEY_TABLE_NAME').AsString + '_' + IntToStr(_FieldsRelations.Count-1) + ';'
     else
       ReadWrite := ' read F' + Metadata.FieldByName('PKEY_TABLE_NAME').AsString + '_' + IntToStr(_FieldsRelations.Count-1) + ' write F' +
                                Metadata.FieldByName('PKEY_TABLE_NAME').AsString + '_' + IntToStr(_FieldsRelations.Count-1) + ';';

      Inc(FK);
     _ForeignKeys.Add(Fields.FieldByName('COLUMN_NAME').AsString + '$' + IntToStr(FK),
                      '    [ForeignKey(''' +Metadata.FieldByName('FKEY_NAME').AsString + ''', ' +
                                           QuotedStr(Fields.FieldByName('COLUMN_NAME').AsString) + ', ' +
                                           QuotedStr(Metadata.FieldByName('PKEY_TABLE_NAME').AsString)+', '+
                                           QuotedStr(Fields.FieldByName('PKEY_COLUMN_NAME').AsString) + ', ' +
                                           GetRule(Metadata.FieldByName('DELETE_RULE').AsInteger) + ', ' +
                                           GetRule(Metadata.FieldByName('UPDATE_RULE').AsInteger) +')]');

     _PropertysRelations.Add('    [Association(OneToOne,' + QuotedStr(Fields.FieldByName('COLUMN_NAME').AsString) + ',' +QuotedStr(Metadata.FieldByName('PKEY_TABLE_NAME').AsString) + ',' + QuotedStr(Fields.FieldByName('PKEY_COLUMN_NAME').AsString) + IfThen(checkLazy.Checked, ', True', EmptyStr) + ')]');
     _PropertysRelations.Add('    property ' +IfThen(checkLowerCase.Checked,
                                                     LowerCase(Metadata.FieldByName('PKEY_TABLE_NAME').AsString),
                                                     Metadata.FieldByName('PKEY_TABLE_NAME').AsString) + ': ' +
                                  'T' + Metadata.FieldByName('PKEY_TABLE_NAME').AsString +
                                  ReadWrite);
     _PropertysRelations.Add('');

     if not checkLazy.Checked then
       _ConstructorImplementation.Add('  F' +Metadata.FieldByName('PKEY_TABLE_NAME').AsString + '_' + IntToStr(_FieldsRelations.Count-1) + ' := T' + Metadata.FieldByName('PKEY_TABLE_NAME').AsString+ '.Create;');

     _DestructorImplementation.Add('  if Assigned(F' + Metadata.FieldByName('PKEY_TABLE_NAME').AsString + '_' + IntToStr(_FieldsRelations.Count-1) + IfThen(checkLazy.Checked, '.Value', EmptyStr) + ') then');
     _DestructorImplementation.Add('    F' + Metadata.FieldByName('PKEY_TABLE_NAME').AsString + '_' + IntToStr(_FieldsRelations.Count-1) + IfThen(checkLazy.Checked, '.Value', EmptyStr) + '.Free;');
     _DestructorImplementation.Add(EmptyStr);

     if checkLazy.Checked then
     begin
       _ConstructorDeclaration.Add('    function get' + Metadata.FieldByName('PKEY_TABLE_NAME').AsString + '_' + IntToStr(_FieldsRelations.Count-1) + ' : T' + Metadata.FieldByName('PKEY_TABLE_NAME').AsString + ';');

       _LazyLoadImplementation.Add('');
       _LazyLoadImplementation.Add('function T' +lstTabelas.Items.Strings[index] + '.get' + Metadata.FieldByName('PKEY_TABLE_NAME').AsString + '_' + IntToStr(_FieldsRelations.Count-1) + ' : T' + Metadata.FieldByName('PKEY_TABLE_NAME').AsString + ';');
       _LazyLoadImplementation.Add('begin');
       _LazyLoadImplementation.Add('  Result := F' + Metadata.FieldByName('PKEY_TABLE_NAME').AsString + '_' + IntToStr(_FieldsRelations.Count-1) + '.Value;');
       _LazyLoadImplementation.Add('end;');
     end;

     Metadata.Next;
  end;

  Entidade.Close;
  Entidade.SQL.Text := 'SELECT * FROM ' + lstTabelas.Items[index];
  Entidade.Open;

  for I := 0 to Entidade.FieldList.Count -1 do
  begin
    if I > 0 then _Propertys.Add('');
      ReadWrite := ' read F'  + Entidade.FieldList.Fields[i].FieldName +
                   ' write F' + Entidade.FieldList.Fields[i].FieldName + ';';

    Campo  := Entidade.FieldList.Fields[i].FieldName;
    L      := Entidade.FieldList.Fields[i].Size;

    if Entidade.FieldByName(Campo).DataType in [ftString, ftWideString] then
       CreateBodyProperty(index,Campo,'String',ReadWrite,'taLeftJustify','''''',I,L)
    else
    if Entidade.FieldByName(Campo).DataType = ftBoolean then
       CreateBodyProperty(index,Campo,'Boolean',ReadWrite,'taLeftJustify','''''',I)
    else
    if Entidade.FieldByName(Campo).DataType in [ftLargeint, ftSmallint, ftInteger, ftWord, ftAutoInc] then
       CreateBodyProperty(index,Campo,'Integer',ReadWrite,'taCenter','''''',I)
    else
    if Entidade.FieldByName(Campo).DataType in [ftFloat, ftCurrency] then
       CreateBodyProperty(index,Campo,'Currency',ReadWrite,'taRightJustify','''0''',I)
    else
    if Entidade.FieldByName(Campo).DataType in [ftTime] then
       CreateBodyProperty(index,Campo,'TTime',ReadWrite,'taCenter','''''',I)
    else
    if Entidade.FieldByName(Campo).DataType in [ftDate, ftDateTime] then
    begin
       if Entidade.FieldList.Fields[i].Required then
          CreateBodyProperty(index,Campo,'TDateTime',ReadWrite,'taCenter','''Date''',I,0,0,0,'''!##/##/####;1;_''')
       else
          CreateBodyProperty(index,Campo,'TDateTime',ReadWrite,'taCenter','''''',I);
    end
    else
    if Entidade.FieldByName(Campo).DataType in [ftTimeStamp] then
    begin
       if Entidade.FieldList.Fields[i].Required then
          CreateBodyProperty(index,Campo,'TDateTime',ReadWrite,'taCenter','''Now''',I,0,0,0,'''!##/##/####;1;_''')
       else
          CreateBodyProperty(index,Campo,'TDateTime',ReadWrite,'taCenter','''''',I);
    end
    else
    if Entidade.FieldByName(Campo).DataType in [ftBlob, ftMemo, ftWideMemo, ftOraBlob, ftOraClob] then
       CreateBodyProperty(index, Campo, 'TBlob', ReadWrite, 'taLeftJustify', '''''', I)
    else
    if Entidade.FieldByName(Campo).DataType in [ftFMTBcd, ftBCD] then
    begin
       L := 0;
       P := TBCDField(Entidade.FieldList.Fields[i]).Precision;
       S := TBCDField(Entidade.FieldList.Fields[i]).Size;
       CreateBodyProperty(index,Campo,'Double',ReadWrite,'taRightJustify','''0''',I,L,P,S);
    end;
  end;
  Entidade.Close;


  if _ConstructorImplementation.Count > 0 then
  begin
    _ConstructorImplementation.Add('end;');
  end;
  if _DestructorImplementation.Count > 0 then
  begin
    _DestructorImplementation.Add('  inherited;');
    _DestructorImplementation.Add('end;');
  end;

end;

procedure TFrmPrincipal.CreateBodyClassUnit(index: Integer);
var
 iKey: Integer;
begin
   memModel.Clear;
  _FieldsProperty.Clear;
  _Propertys.Clear;
  _FieldsRelations.Clear;
  _PropertysRelations.Clear;
  _ConstructorDeclaration.Clear;
  _DestructorDeclaration.Clear;
  _ConstructorImplementation.Clear;
  _DestructorImplementation.Clear;
  _UsesRelations.Clear;
  _Declaration.Clear;
  _Implementation.Clear;
  _ForeignKeys.Clear;

  //Carrega propriedades
  LoadPropertys(index);
  //
  memModel.Lines.Add('unit '+edtProjeto.Text+LowerCase( lstTabelas.Items.Strings[index] )+ ';');
  memModel.Lines.Add('');
  memModel.Lines.Add('interface');
  memModel.Lines.Add('');
  memModel.Lines.Add('uses');
  memModel.Lines.Add('  DB, ');
  memModel.Lines.Add('  Classes, ');
  memModel.Lines.Add('  SysUtils, ');
  memModel.Lines.Add('  Generics.Collections, ');
  memModel.Lines.Add('');
  memModel.Lines.Add('  /// orm ');
  //Add Units Associadas
  for iKey  := 0 to _UsesRelations.Count -1 do
    memModel.Lines.Add(_UsesRelations[iKey] + ',');
  memModel.Lines.Add('  ormbr.types.blob, ');
  memModel.Lines.Add('  ormbr.types.lazy, ');
  memModel.Lines.Add('  ormbr.types.mapping, ');
  memModel.Lines.Add('  ormbr.types.nullable, ');
  memModel.Lines.Add('  ormbr.mapping.classes, ');
  memModel.Lines.Add('  ormbr.mapping.register, ');
  memModel.Lines.Add('  ormbr.mapping.attributes; ');
  memModel.Lines.Add('');
  memModel.Lines.Add('type');
  memModel.Lines.Add('  [Entity]');
  memModel.Lines.Add('  [Table(' + QuotedStr(lstTabelas.Items.Strings[index]) + ', '''')]');

  for iKey  := 0 to _PrimaryKey.Count -1 do
  memModel.Lines.Add(_PrimaryKey[iKey]);

  for iKey  := 0 to _Indexes.Count -1 do
  memModel.Lines.Add(_Indexes[iKey]);

  for iKey  := 0 to _Checks.Count -1 do
  memModel.Lines.Add(_Checks[iKey]);

  memModel.Lines.Add('  T' + lstTabelas.Items.Strings[index] + ' = class');
  memModel.Lines.Add('  private');
  memModel.Lines.Add('    { Private declarations } ');
  //Add Var Field
  for iKey  := 0 to _FieldsProperty.Count -1 do
      memModel.Lines.Add(_FieldsProperty.Strings[iKey]);

  //Add Field de Ralacionamentos
  if _FieldsRelations.Count > 0 then
  begin
    memModel.Lines.Add('');
    for iKey  := 0 to _FieldsRelations.Count -1 do
    memModel.Lines.Add(_FieldsRelations.Strings[ikey]);
  end;
  memModel.Lines.Add('  public ');
  memModel.Lines.Add('    { Public declarations } ');
  //Add Declara��es dos Metodos
  if _ConstructorDeclaration.Count > 0 then
  begin
     for iKey  := 0 to _ConstructorDeclaration.Count -1 do
     memModel.Lines.Add(_ConstructorDeclaration.Strings[ikey]);
  end;
  if _DestructorDeclaration.Count > 0 then
  begin
     for iKey  := 0 to _DestructorDeclaration.Count -1 do
     memModel.Lines.Add(_DestructorDeclaration.Strings[ikey]);
  end;

  // Add Propriedades Fields
  for iKey  := 0 to _Propertys.Count -1 do
  memModel.Lines.Add(_Propertys.Strings[iKey]);

  // Add Propriedades de rela��o
  if _PropertysRelations.count > 0 then
  begin
    memModel.Lines.Add('');
    for iKey  := 0 to _PropertysRelations.Count -1 do
    memModel.Lines.Add(_PropertysRelations.Strings[iKey]);
  end;
  memModel.Lines.Add('  end;');

  memModel.Lines.Add('');
  memModel.Lines.Add('implementation');

  //Add Implemeta��o dos Metodos Declarados
  if _ConstructorImplementation.Count > 0 then
  begin
     for iKey  := 0 to _ConstructorImplementation.Count -1 do
     memModel.Lines.Add(_ConstructorImplementation.Strings[iKey]);
  end;
  if _DestructorImplementation.Count > 0 then
  begin
     for iKey  := 0 to _DestructorImplementation.Count -1 do
     memModel.Lines.Add(_DestructorImplementation.Strings[iKey]);
  end;

  if _LazyLoadImplementation.Count > 0 then
  begin
     for iKey  := 0 to _LazyLoadImplementation.Count -1 do
     memModel.Lines.Add(_LazyLoadImplementation.Strings[iKey]);
  end;

  memModel.Lines.Add('');
  memModel.Lines.Add('initialization');
  memModel.Lines.Add('  TRegisterClass.RegisterEntity(T'+lstTabelas.Items.Strings[index]+')');
  memModel.Lines.Add('');

  memModel.Lines.Add('end.');
end;



procedure TFrmPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   CDS_CNN.SaveToFile(ExtractFilePath(ParamStr(0))+'Connection.xml', dfXML);
end;

procedure TFrmPrincipal.FormCreate(Sender: TObject);
begin
  FAppPathName := ExtractFilePath(ParamStr(0));

  //Abre Configura��es de Conex�o
  LoadConnection;

  tabModel.Show;
end;


end.

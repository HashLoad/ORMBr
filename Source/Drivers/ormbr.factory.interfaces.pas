{
      ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Versão 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos é permitido copiar e distribuir cópias deste documento de
       licença, mas mudá-lo não é permitido.

       Esta versão da GNU Lesser General Public License incorpora
       os termos e condições da versão 3 da GNU General Public License
       Licença, complementado pelas permissões adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.factory.interfaces;

interface

uses
  DB,
  Classes,
  SysUtils,
  Variants,
  ormbr.monitor;

type
  TDriverName = (dnMSSQL, dnMySQL, dnFirebird, dnSQLite, dnInterbase, dnDB2,
                 dnOracle, dnInformix, dnPostgreSQL, dnADS, dnASA,
                 dnAbsoluteDB, dnMongoDB);

  TAsField = class abstract
  protected
    FAsFieldName: String;
  public
    function IsNull: Boolean; virtual; abstract;
    function AsBlob: TMemoryStream; virtual; abstract;
    function AsBlobPtr(out iNumBytes: Integer): Pointer; virtual; abstract;
    function AsBlobText: string; virtual; abstract;
    function AsBlobTextDef(const Def: string = ''): string; virtual; abstract;
    function AsDateTime: TDateTime; virtual; abstract;
    function AsDateTimeDef(const Def: TDateTime = 0.0): TDateTime; virtual; abstract;
    function AsDouble: Double; virtual; abstract;
    function AsDoubleDef(const Def: Double = 0.0): Double; virtual; abstract;
    function AsInteger: Int64; virtual; abstract;
    function AsIntegerDef(const Def: Int64 = 0): Int64; virtual; abstract;
    function AsString: string; virtual; abstract;
    function AsStringDef(const Def: string = ''): string; virtual; abstract;
    function AsFloat: Double; virtual; abstract;
    function AsFloatDef(const Def: Double = 0): Double; virtual; abstract;
    function AsCurrency: Currency; virtual; abstract;
    function AsCurrencyDef(const Def: Currency = 0): Currency; virtual; abstract;
    function AsExtended: Extended; virtual; abstract;
    function AsExtendedDef(const Def: Extended = 0): Extended; virtual; abstract;
    function AsVariant: Variant; virtual; abstract;
    function AsVariantDef(const Def: Variant): Variant; virtual; abstract;
    function AsBoolean: Boolean; virtual; abstract;
    function AsBooleanDef(const Def: Boolean = False): Boolean; virtual; abstract;
    function Value: Variant; virtual; abstract;
    function ValueDef(const Def: Variant): Variant; virtual; abstract;
    property AsFieldName: String read FAsFieldName write FAsFieldName;
  end;

  /// <summary>
  /// Unit : ormbr.driver.connection.pas
  /// Classe : TDriverResultSet<T: TDataSet>
  /// </summary>
  IDBResultSet = interface
    ['{A8ECADF6-A9AF-4610-8429-3B0A5CD0295C}']
    function GetFetchingAll: Boolean;
    procedure SetFetchingAll(const Value: Boolean);
    procedure Close;
    function NotEof: Boolean;
    function RecordCount: Integer;
    function FieldDefs: TFieldDefs;
    function GetFieldValue(AFieldName: string): Variant; overload;
    function GetFieldValue(AFieldIndex: Integer): Variant; overload;
    function GetField(AFieldName: string): TField;
    function GetFieldType(AFieldName: string): TFieldType;
    function FieldByName(AFieldName: string): TAsField;
    function DataSet: TDataSet;
    property FetchingAll: Boolean read GetFetchingAll write SetFetchingAll;
  end;

  IDBQuery = interface
    ['{0588C65B-2571-48BB-BE03-BD51ABB6897F}']
    procedure SetCommandText(ACommandText: string);
    function GetCommandText: string;
    procedure ExecuteDirect;
    function ExecuteQuery: IDBResultSet;
    property CommandText: string read GetCommandText write SetCommandText;
  end;

  IDBConnection = interface
    ['{4520C97F-8777-4D14-9C14-C79EF86957DB}']
    procedure Connect;
    procedure Disconnect;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    procedure ExecuteDirect(const ASQL: string); overload;
    procedure ExecuteDirect(const ASQL: string; const AParams: TParams); overload;
    procedure ExecuteScript(const ASQL: string);
    procedure AddScript(const ASQL: string);
    procedure ExecuteScripts;
    procedure SetCommandMonitor(AMonitor: ICommandMonitor);
    function InTransaction: Boolean;
    function IsConnected: Boolean;
    function GetDriverName: TDriverName;
    function CreateQuery: IDBQuery;
    function CreateResultSet: IDBResultSet;
    function ExecuteSQL(const ASQL: string): IDBResultSet;
    function CommandMonitor: ICommandMonitor;
  end;

  IDBTransaction = interface
    ['{EB46599C-A021-40E4-94E2-C7507781562B}']
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function InTransaction: Boolean;
  end;

implementation

end.

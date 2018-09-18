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
  TAsField = class;

  TDriverName = (dnMSSQL, dnMySQL, dnFirebird, dnSQLite, dnInterbase, dnDB2,
                 dnOracle, dnInformix, dnPostgreSQL, dnADS, dnASA,
                 dnAbsoluteDB, dnMongoDB);
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
    function FieldByName(AFieldName: string): TAsField; //IDBResultSet;
//    function AsString: string;
//    function AsInteger: Integer;
//    function AsFloat: Double;
//    function AsCurrency: Currency;
//    function AsExtended: Extended;
//    function AsDateTime: TDateTime;
//    function AsVariant: Variant;
//    function AsBoolean: Boolean;
//    function Value: Variant;
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

  TAsField = class
  private
    FOwner: IDBResultSet;
  public
    FAsFieldName: String;
    constructor Create(AOwner: IDBResultSet);
    function IsNull: Boolean;
    function AsBlob: TMemoryStream;
    function AsBlobPtr(out iNumBytes: Integer): Pointer;
    function AsBlobText: string;
    function AsBlobTextDef(const Def: string = ''): string;
    function AsDateTime: TDateTime;
    function AsDateTimeDef(const Def: TDateTime = 0.0): TDateTime;
    function AsDouble: Double;
    function AsDoubleDef(const Def: Double = 0.0): Double;
    function AsInteger: Int64;
    function AsIntegerDef(const Def: Int64 = 0): Int64;
    function AsString: string;
    function AsStringDef(const Def: string = ''): string;
    function AsFloat: Double;
    function AsFloatDef(const Def: Double = 0): Double;
    function AsCurrency: Currency;
    function AsCurrencyDef(const Def: Currency = 0): Currency;
    function AsExtended: Extended;
    function AsExtendedDef(const Def: Extended = 0): Extended;
    function AsVariant: Variant;
    function AsVariantDef(const Def: Variant): Variant;
    function AsBoolean: Boolean;
    function AsBooleanDef(const Def: Boolean = False): Boolean;
    function Value: Variant;
    function ValueDef(const Def: Variant): Variant;
  end;

implementation

{ TAsField }

constructor TAsField.Create(AOwner: IDBResultSet);
begin
  FOwner := AOwner;
end;

function TAsField.AsBlob: TMemoryStream;
begin
//  Result := TMemoryStream( FOwner.GetFieldValue(FAsFieldName) );
end;

function TAsField.AsBlobPtr(out iNumBytes: Integer): Pointer;
begin
//  Result := Pointer( FOwner.GetFieldValue(FAsFieldName) );
end;

function TAsField.AsBlobText: string;
var
  LResult: Variant;
begin
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult = Null then
    Exit;
  Result := String(LResult);
end;

function TAsField.AsBlobTextDef(const Def: string): string;
begin
  try
    Result := String(FOwner.GetFieldValue(FAsFieldName));
  except
    Result := Def;
  end;
end;

function TAsField.AsBoolean: Boolean;
var
  LResult: Variant;
begin
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult = Null then
    Exit;
  Result := Boolean(LResult);
end;

function TAsField.AsBooleanDef(const Def: Boolean): Boolean;
begin
  try
    Result := Boolean(FOwner.GetFieldValue(FAsFieldName));
  except
    Result := Def;
  end;
end;

function TAsField.AsCurrency: Currency;
var
  LResult: Variant;
begin
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult = Null then
    Exit;
  Result := Currency(LResult);
end;

function TAsField.AsCurrencyDef(const Def: Currency): Currency;
begin
  try
    Result := Currency(FOwner.GetFieldValue(FAsFieldName));
  except
    Result := Def;
  end;
end;

function TAsField.AsDateTime: TDateTime;
var
  LResult: Variant;
begin
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult = Null then
    Exit;
  Result := TDateTime(LResult);
end;

function TAsField.AsDateTimeDef(const Def: TDateTime): TDateTime;
begin
  try
    Result := TDateTime( FOwner.GetFieldValue(FAsFieldName) );
  except
    Result := Def;
  end;
end;

function TAsField.AsDouble: Double;
var
  LResult: Variant;
begin
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult = Null then
    Exit;
  Result := Double(LResult);
end;

function TAsField.AsDoubleDef(const Def: Double): Double;
begin
  try
    Result := Double(FOwner.GetFieldValue(FAsFieldName));
  except
    Result := Def;
  end;
end;

function TAsField.AsExtended: Extended;
var
  LResult: Variant;
begin
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult = Null then
    Exit;
  Result := Extended(LResult);
end;

function TAsField.AsExtendedDef(const Def: Extended): Extended;
begin
  try
    Result := Extended(FOwner.GetFieldValue(FAsFieldName));
  except
    Result := Def;
  end;
end;

function TAsField.AsFloat: Double;
var
  LResult: Variant;
begin
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult = Null then
    Exit;
  Result := Double(LResult);
end;

function TAsField.AsFloatDef(const Def: Double): Double;
begin
  try
    Result := Double(FOwner.GetFieldValue(FAsFieldName));
  except
    Result := Def;
  end;
end;

function TAsField.AsInteger: Int64;
var
  LResult: Variant;
begin
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult = Null then
    Exit;
  Result := Int64(LResult);
end;

function TAsField.AsIntegerDef(const Def: Int64): Int64;
begin
  try
    Result := Int64(FOwner.GetFieldValue(FAsFieldName));
  except
    Result := Def;
  end;
end;

function TAsField.AsString: string;
var
  LResult: Variant;
begin
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult = Null then
    Exit;
  Result := String(LResult);
end;

function TAsField.AsStringDef(const Def: string): string;
begin
  try
    Result := String(FOwner.GetFieldValue(FAsFieldName));
  except
    Result := Def;
  end;
end;

function TAsField.AsVariant: Variant;
var
  LResult: Variant;
begin
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult = Null then
    Exit;
  Result := LResult;
end;

function TAsField.AsVariantDef(const Def: Variant): Variant;
begin
  try
    Result := FOwner.GetFieldValue(FAsFieldName);
  except
    Result := Def;
  end;
end;

function TAsField.IsNull: Boolean;
begin
  Result := FOwner.GetFieldValue(FAsFieldName) = Null;
end;

function TAsField.Value: Variant;
var
  LResult: Variant;
begin
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult = Null then
    Exit;
  Result := LResult;
end;

function TAsField.ValueDef(const Def: Variant): Variant;
begin
  try
    Result := FOwner.GetFieldValue(FAsFieldName);
  except
    Result := Def;
  end;
end;

end.

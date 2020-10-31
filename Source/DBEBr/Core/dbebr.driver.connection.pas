{
  DBE Brasil é um Engine de Conexão simples e descomplicado for Delphi/Lazarus

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

{ @abstract(DBEBr Framework)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <https://www.isaquepinheiro.com.br>)
}

unit dbebr.driver.connection;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  DB,
  Math,
  Classes,
  SysUtils,
  Variants,
  // DBEBr
  dbebr.factory.interfaces;

type
  // Classe de conexões abstract
  TDriverConnection = class abstract
  protected
    FDriverName: TDriverName;
  public
    constructor Create(const AConnection: TComponent;
      const ADriverName: TDriverName); virtual; abstract;
    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    procedure ExecuteDirect(const ASQL: string); overload; virtual; abstract;
    procedure ExecuteDirect(const ASQL: string;
      const AParams: TParams); overload; virtual; abstract;
    procedure ExecuteScript(const ASQL: string); virtual; abstract;
    procedure AddScript(const ASQL: string); virtual; abstract;
    procedure ExecuteScripts; virtual; abstract;
    function IsConnected: Boolean; virtual; abstract;
    function InTransaction: Boolean; virtual; abstract;
    function CreateQuery: IDBQuery; virtual; abstract;
    function CreateResultSet(const ASQL: string): IDBResultSet; virtual; abstract;
    function ExecuteSQL(const ASQL: string): IDBResultSet; virtual; abstract;
    property DriverName: TDriverName read FDriverName;
  end;

  // Classe de trasações abstract
  TDriverTransaction = class abstract(TInterfacedObject, IDBTransaction)
  public
    constructor Create(AConnection: TComponent); virtual; abstract;
    procedure StartTransaction; virtual; abstract;
    procedure Commit; virtual; abstract;
    procedure Rollback; virtual; abstract;
    function InTransaction: Boolean; virtual; abstract;
  end;

  TDriverQuery = class(TInterfacedObject, IDBQuery)
  protected
    procedure SetCommandText(ACommandText: string); virtual; abstract;
    function GetCommandText: string; virtual; abstract;
  public
    procedure ExecuteDirect; virtual; abstract;
    function ExecuteQuery: IDBResultSet; virtual; abstract;
    property CommandText: string read GetCommandText write SetCommandText;
  end;

  TDriverResultSetBase = class(TInterfacedObject, IDBResultSet)
  private
    function GetFetchingAll: Boolean;
    procedure SetFetchingAll(const Value: Boolean);
  protected
    FField: TAsField;
    FFieldNameInternal: string;
    FRecordCount: Integer;
    FFetchingAll: Boolean;
    FFirstNext: Boolean;
  public
    constructor Create; overload; virtual;
    destructor Destroy; override;
    procedure Close; virtual; abstract;
    function NotEof: Boolean; virtual; abstract;
    function GetFieldValue(const AFieldName: string): Variant; overload; virtual; abstract;
    function GetFieldValue(const AFieldIndex: Integer): Variant; overload; virtual; abstract;
    function GetFieldType(const AFieldName: string): TFieldType; overload; virtual; abstract;
    function GetField(const AFieldName: string): TField; virtual; abstract;
    function FieldByName(const AFieldName: string): TAsField; virtual;
    function RecordCount: Integer; virtual;
    function FieldDefs: TFieldDefs; virtual; abstract;
    function DataSet: TDataSet; virtual; abstract;
    property FetchingAll: Boolean read GetFetchingAll write SetFetchingAll;
  end;

  TDriverResultSet<T: TDataSet> = class abstract(TDriverResultSetBase)
  protected
    FDataSet: T;
  public
    constructor Create(ADataSet: T); overload; virtual;
    procedure Close; override;
    function FieldDefs: TFieldDefs; override;
    function DataSet: TDataSet; override;
  end;

  TDBEBrField = class(TAsField)
  private
    FOwner: TDriverResultSetBase;
  public
    constructor Create(AOwner: TDriverResultSetBase);
    destructor Destroy; override;
    function IsNull: Boolean; override;
    function AsBlob: TMemoryStream; override;
    function AsBlobPtr(out iNumBytes: Integer): Pointer; override;
    function AsBlobText: string; override;
    function AsBlobTextDef(const Def: string = ''): string; override;
    function AsDateTime: TDateTime; override;
    function AsDateTimeDef(const Def: TDateTime = 0.0): TDateTime; override;
    function AsDouble: Double; override;
    function AsDoubleDef(const Def: Double = 0.0): Double; override;
    function AsInteger: Int64; override;
    function AsIntegerDef(const Def: Int64 = 0): Int64; override;
    function AsString: string; override;
    function AsStringDef(const Def: string = ''): string; override;
    function AsFloat: Double; override;
    function AsFloatDef(const Def: Double = 0): Double; override;
    function AsCurrency: Currency; override;
    function AsCurrencyDef(const Def: Currency = 0): Currency; override;
    function AsExtended: Extended; override;
    function AsExtendedDef(const Def: Extended = 0): Extended; override;
    function AsVariant: Variant; override;
    function AsVariantDef(const Def: Variant): Variant; override;
    function AsBoolean: Boolean; override;
    function AsBooleanDef(const Def: Boolean = False): Boolean; override;
    function Value: Variant; override;
    function ValueDef(const Def: Variant): Variant; override;
  end;

implementation

{ TDriverResultSet<T> }

constructor TDriverResultSet<T>.Create(ADataSet: T);
begin
  Create;
  // Guarda RecordCount do último SELECT executado no IDBResultSet
  try
  FRecordCount := FDataSet.RecordCount;
  except
  end;
end;

function TDriverResultSet<T>.DataSet: TDataSet;
begin
  Result := FDataSet;
end;

procedure TDriverResultSet<T>.Close;
begin
  inherited;
  FDataSet.Close;
end;

function TDriverResultSet<T>.FieldDefs: TFieldDefs;
begin
  inherited;
  Result := FDataSet.FieldDefs;
end;

{ TDriverResultSetBase }

function TDriverResultSetBase.RecordCount: Integer;
begin
  Result := FRecordCount;
end;

constructor TDriverResultSetBase.Create;
begin
  FField := TDBEBrField.Create(Self);
end;

destructor TDriverResultSetBase.Destroy;
begin
  FField.Free;
  inherited;
end;

function TDriverResultSetBase.FieldByName(const AFieldName: string): TAsField;
begin
  FField.AsFieldName := AFieldName;
  Result := FField;
end;

function TDriverResultSetBase.GetFetchingAll: Boolean;
begin
  Result := FFetchingAll;
end;

procedure TDriverResultSetBase.SetFetchingAll(const Value: Boolean);
begin
  FFetchingAll := Value;
end;

{ TAsField }

constructor TDBEBrField.Create(AOwner: TDriverResultSetBase);
begin
  FOwner := AOwner;
end;

destructor TDBEBrField.Destroy;
begin
  FOwner := nil;
  inherited;
end;

function TDBEBrField.AsBlob: TMemoryStream;
begin
//  Result := TMemoryStream( FOwner.GetFieldValue(FAsFieldName) );
  Result := nil;
end;

function TDBEBrField.AsBlobPtr(out iNumBytes: Integer): Pointer;
begin
//  Result := Pointer( FOwner.GetFieldValue(FAsFieldName) );
  Result := nil;
end;

function TDBEBrField.AsBlobText: string;
var
  LResult: Variant;
begin
  Result := '';
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult <> Null then
    Result := String(LResult);
end;

function TDBEBrField.AsBlobTextDef(const Def: string): string;
begin
  try
    Result := String(FOwner.GetFieldValue(FAsFieldName));
  except
    Result := Def;
  end;
end;

function TDBEBrField.AsBoolean: Boolean;
var
  LResult: Variant;
begin
  Result := False;
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult <> Null then
    Result := Boolean(Value);
end;

function TDBEBrField.AsBooleanDef(const Def: Boolean): Boolean;
begin
  try
    Result := Boolean(FOwner.GetFieldValue(FAsFieldName));
  except
    Result := Def;
  end;
end;

function TDBEBrField.AsCurrency: Currency;
var
  LResult: Variant;
begin
  Result := 0;
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult <> Null then
    Result := Currency(LResult);
end;

function TDBEBrField.AsCurrencyDef(const Def: Currency): Currency;
begin
  try
    Result := Currency(FOwner.GetFieldValue(FAsFieldName));
  except
    Result := Def;
  end;
end;

function TDBEBrField.AsDateTime: TDateTime;
var
  LResult: Variant;
begin
  Result := 0;
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult <> Null then
    Result := TDateTime(LResult);
end;

function TDBEBrField.AsDateTimeDef(const Def: TDateTime): TDateTime;
begin
  try
    Result := TDateTime( FOwner.GetFieldValue(FAsFieldName) );
  except
    Result := Def;
  end;
end;

function TDBEBrField.AsDouble: Double;
var
  LResult: Variant;
begin
  Result := 0;
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult <> Null then
    Result := Double(LResult);
end;

function TDBEBrField.AsDoubleDef(const Def: Double): Double;
begin
  try
    Result := Double(FOwner.GetFieldValue(FAsFieldName));
  except
    Result := Def;
  end;
end;

function TDBEBrField.AsExtended: Extended;
var
  LResult: Variant;
begin
  Result := 0;
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult <> Null then
    Result := Extended(LResult);
end;

function TDBEBrField.AsExtendedDef(const Def: Extended): Extended;
begin
  try
    Result := Extended(FOwner.GetFieldValue(FAsFieldName));
  except
    Result := Def;
  end;
end;

function TDBEBrField.AsFloat: Double;
var
  LResult: Variant;
begin
  Result := 0;
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult <> Null then
    Result := Double(LResult);
end;

function TDBEBrField.AsFloatDef(const Def: Double): Double;
begin
  try
    Result := Double(FOwner.GetFieldValue(FAsFieldName));
  except
    Result := Def;
  end;
end;

function TDBEBrField.AsInteger: Int64;
var
  LResult: Variant;
begin
  Result := 0;
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult <> Null then
    Result := Int64(LResult);
end;

function TDBEBrField.AsIntegerDef(const Def: Int64): Int64;
begin
  try
    Result := Int64(FOwner.GetFieldValue(FAsFieldName));
  except
    Result := Def;
  end;
end;

function TDBEBrField.AsString: string;
var
  LResult: Variant;
begin
  Result := '';
  LResult := FOwner.GetFieldValue(FAsFieldName);
  if LResult <> Null then
    Result := String(LResult);
end;

function TDBEBrField.AsStringDef(const Def: string): string;
begin
  try
    Result := String(FOwner.GetFieldValue(FAsFieldName));
  except
    Result := Def;
  end;
end;

function TDBEBrField.AsVariant: Variant;
begin
  Result := FOwner.GetFieldValue(FAsFieldName);
end;

function TDBEBrField.AsVariantDef(const Def: Variant): Variant;
begin
  try
    Result := FOwner.GetFieldValue(FAsFieldName);
  except
    Result := Def;
  end;
end;

function TDBEBrField.IsNull: Boolean;
begin
  Result := FOwner.GetFieldValue(FAsFieldName) = Null;
end;

function TDBEBrField.Value: Variant;
begin
  Result := FOwner.GetFieldValue(FAsFieldName);
end;

function TDBEBrField.ValueDef(const Def: Variant): Variant;
begin
  try
    Result := FOwner.GetFieldValue(FAsFieldName);
  except
    Result := Def;
  end;
end;

end.

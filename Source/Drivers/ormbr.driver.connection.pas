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

unit ormbr.driver.connection;

interface

uses
  Classes,
  DB,
  Variants,
  ormbr.types.database,
  ormbr.factory.interfaces;

type
  /// <summary>
  /// Classe de conexões abstract
  /// </summary>
  TDriverConnection = class abstract
  protected
    FDriverName: TDriverName;
  public
    constructor Create(AConnection: TComponent; ADriverName: TDriverName); virtual; abstract;
    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    procedure ExecuteDirect(const ASQL: string); overload; virtual; abstract;
    procedure ExecuteDirect(const ASQL: string; const AParams: TParams); overload; virtual; abstract;
    procedure ExecuteScript(const ASQL: string); virtual; abstract;
    procedure AddScript(const ASQL: string); virtual; abstract;
    procedure ExecuteScripts; virtual; abstract;
    function IsConnected: Boolean; virtual; abstract;
    function InTransaction: Boolean; virtual; abstract;
    function CreateQuery: IDBQuery; virtual; abstract;
    function CreateResultSet: IDBResultSet; virtual; abstract;
    function ExecuteSQL(const ASQL: string): IDBResultSet; virtual; abstract;
    property DriverName: TDriverName read FDriverName;
  end;

  /// <summary>
  /// Classe de trasações abstract
  /// </summary>
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
    FField: TAsField;
    function GetFetchingAll: Boolean;
    procedure SetFetchingAll(const Value: Boolean);
  protected
    FFieldNameInternal: string;
    FRecordCount: Integer;
    FFetchingAll: Boolean;
    FFirstNext: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Close; virtual; abstract;
    function FieldDefs: TFieldDefs; virtual; abstract;
    function NotEof: Boolean; virtual; abstract;
    function GetFieldValue(AFieldName: string): Variant; overload; virtual; abstract;
    function GetFieldValue(AFieldIndex: Integer): Variant; overload; virtual; abstract;
    function GetFieldType(AFieldName: string): TFieldType; overload; virtual; abstract;
    function GetField(AFieldName: string): TField; virtual; abstract;
    function RecordCount: Integer; virtual;
    function FieldByName(AFieldName: string): TAsField;{ IDBResultSet; }virtual;
//    function AsString: string; virtual;
//    function AsInteger: Integer; virtual;
//    function AsFloat: Double; virtual;
//    function AsCurrency: Currency; virtual;
//    function AsExtended: Extended; virtual;
//    function AsDateTime: TDateTime; virtual;
//    function AsVariant: Variant; virtual;
//    function AsBoolean: Boolean; virtual;
//    function Value: Variant; virtual;
    function DataSet: TDataSet; virtual; abstract;
    property FetchingAll: Boolean read GetFetchingAll write SetFetchingAll;
  end;

  TDriverResultSet<T: TDataSet> = class abstract(TDriverResultSetBase)
  protected
    FDataSet: T;
  public
    constructor Create(ADataSet: T); virtual;
    destructor Destroy; override;
    procedure Close; override;
    function FieldDefs: TFieldDefs; override;
    function DataSet: TDataSet; override;
  end;

implementation

uses
  SysUtils;

{ TDriverResultSet<T> }

constructor TDriverResultSet<T>.Create(ADataSet: T);
begin
  /// <summary>
  /// Guarda RecordCount do último SELECT executado no IDBResultSet
  /// </summary>
  try
  FRecordCount := FDataSet.RecordCount;
  except
  end;
end;

destructor TDriverResultSet<T>.Destroy;
begin
  inherited;
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

function TDriverResultSet<T>.DataSet: TDataSet;
begin
  Result := FDataSet;
end;

{ TDriverResultSetBase }

//function TDriverResultSetBase.AsBoolean: Boolean;
//var
//  LResult: Variant;
//begin
//  Result := False;
//  if Length(FFieldNameInternal) = 0 then
//    raise Exception.Create('Este método só pode ser usado em sequência ao método FieldByName().AsBoolean');
//  LResult := GetFieldValue(FFieldNameInternal);
//  if LResult = Null then
//    Exit;
//  try
//    try
//      Result := Boolean(LResult);
//    except
//      on E: Exception do
//      begin
//        raise Exception.Create('Error de conversão de tipo!');
//      end;
//    end;
//  finally
//    FFieldNameInternal := '';
//  end;
//end;
//
//function TDriverResultSetBase.AsExtended: Extended;
//var
//  LResult: Variant;
//begin
//  Result := 0;
//  if Length(FFieldNameInternal) = 0 then
//    raise Exception.Create('Este método só pode ser usado em sequência ao método FieldByName().AsExtended');
//  LResult := GetFieldValue(FFieldNameInternal);
//  if LResult = Null then
//    Exit;
//  try
//    try
//      Result := Extended(LResult);
//    except
//      on E: Exception do
//      begin
//        raise Exception.Create('Error de conversão de tipo!');
//      end;
//    end;
//  finally
//    FFieldNameInternal := '';
//  end;
//end;
//
//function TDriverResultSetBase.AsCurrency: Currency;
//var
//  LResult: Variant;
//begin
//  Result := 0;
//  if Length(FFieldNameInternal) = 0 then
//    raise Exception.Create('Este método só pode ser usado em sequência ao método FieldByName().AsCurrency');
//  LResult := GetFieldValue(FFieldNameInternal);
//  if LResult = Null then
//    Exit;
//  try
//    try
//      Result := Currency(LResult);
//    except
//      on E: Exception do
//      begin
//        raise Exception.Create('Error de conversão de tipo!');
//      end;
//    end;
//  finally
//    FFieldNameInternal := '';
//  end;
//end;
//
//function TDriverResultSetBase.AsDateTime: TDateTime;
//var
//  LResult: Variant;
//begin
//  Result := 0;
//  if Length(FFieldNameInternal) = 0 then
//    raise Exception.Create('Este método só pode ser usado em sequência ao método FieldByName().AsDateTime');
//  LResult := GetFieldValue(FFieldNameInternal);
//  if LResult = Null then
//    Exit;
//  try
//    try
//      Result := TDateTime(LResult);
//    except
//      on E: Exception do
//      begin
//        raise Exception.Create('Error de conversão de tipo!');
//      end;
//    end;
//  finally
//    FFieldNameInternal := '';
//  end;
//end;
//
//function TDriverResultSetBase.AsFloat: Double;
//var
//  LResult: Variant;
//begin
//  Result := 0;
//  if Length(FFieldNameInternal) = 0 then
//    raise Exception.Create('Este método só pode ser usado em sequência ao método FieldByName().AsFloat');
//  LResult := GetFieldValue(FFieldNameInternal);
//  if LResult = Null then
//    Exit;
//  try
//    try
//      Result := Double(LResult);
//    except
//      on E: Exception do
//      begin
//        raise Exception.Create('Error de conversão de tipo!');
//      end;
//    end;
//  finally
//    FFieldNameInternal := '';
//  end;
//end;
//
//function TDriverResultSetBase.AsInteger: Integer;
//var
//  LResult: Variant;
//begin
//  Result := 0;
//  if Length(FFieldNameInternal) = 0 then
//    raise Exception.Create('Este método só pode ser usado em sequência ao método FieldByName().AsInteger');
//  LResult := GetFieldValue(FFieldNameInternal);
//  if LResult = Null then
//    Exit;
//  try
//    try
//      Result := Integer(LResult);
//    except
//      on E: Exception do
//      begin
//        raise Exception.Create('Error de conversão de tipo!');
//      end;
//    end;
//  finally
//    FFieldNameInternal := '';
//  end;
//end;
//
//function TDriverResultSetBase.AsString: String;
//var
//  LResult: Variant;
//begin
//  Result := '';
//  if Length(FFieldNameInternal) = 0 then
//    raise Exception.Create('Este método só pode ser usado em sequência ao método FieldByName().AsString');
//  LResult := GetFieldValue(FFieldNameInternal);
//  if LResult = Null then
//    Exit;
//  try
//    try
//      Result := String(LResult);
//    except
//      on E: Exception do
//      begin
//        raise Exception.Create('Error de conversão de tipo!');
//      end;
//    end;
//  finally
//    FFieldNameInternal := '';
//  end;
//end;
//
//function TDriverResultSetBase.AsVariant: Variant;
//var
//  LResult: Variant;
//begin
//  Result := Null;
//  if Length(FFieldNameInternal) = 0 then
//    raise Exception.Create('Este método só pode ser usado em sequência ao método FieldByName().AsVariant');
//  LResult := GetFieldValue(FFieldNameInternal);
//  if LResult = Null then
//    Exit;
//  try
//    try
//      Result := LResult;
//    except
//      on E: Exception do
//      begin
//        raise Exception.Create('Error de conversão de tipo!');
//      end;
//    end;
//  finally
//    FFieldNameInternal := '';
//  end;
//end;

constructor TDriverResultSetBase.Create;
begin
  FField := TAsField.Create(Self);
end;

destructor TDriverResultSetBase.Destroy;
begin
  FField.Free;
  inherited;
end;

function TDriverResultSetBase.FieldByName(AFieldName: string): TAsField; //IDBResultSet;
begin
//  FFieldNameInternal := AFieldName;
//  Result := Self;
  FField.FAsFieldName := AFieldName;
  Result := FField;
end;

function TDriverResultSetBase.RecordCount: Integer;
begin
  Result := FRecordCount;
end;

//function TDriverResultSetBase.Value: Variant;
//var
//  LResult: Variant;
//begin
//  if Length(FFieldNameInternal) = 0 then
//    raise Exception.Create('Este método só pode ser usado em sequência ao método FieldByName().Value');
//  LResult := GetFieldValue(FFieldNameInternal);
//  if LResult = Null then
//    Exit(Null);
//  try
//    try
//      Result := LResult;
//    except
//      on E: Exception do
//      begin
//        raise Exception.Create('Error de conversão de tipo!');
//      end;
//    end;
//  finally
//    FFieldNameInternal := '';
//  end;
//end;

function TDriverResultSetBase.GetFetchingAll: Boolean;
begin
  Result := FFetchingAll;
end;

procedure TDriverResultSetBase.SetFetchingAll(const Value: Boolean);
begin
  FFetchingAll := Value;
end;

end.

{
         CQL Brasil - Criteria Query Language for Delphi/Lazarus


                   Copyright (c) 2019, Isaque Pinheiro
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

{ @abstract(CQLBr Framework)
  @created(18 Jul 2019)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @author(Site : https://www.isaquepinheiro.com.br)
}

unit cqlbr.core;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  SysUtils,
  Generics.Collections,
  cqlbr.interfaces;

type
  TCQLSection = class(TInterfacedObject, ICQLSection)
  private
    FName: String;
  protected
    function GetName: String;
  public
    constructor Create(ASectionName: String);
    procedure Clear; virtual; abstract;
    function IsEmpty: Boolean; virtual; abstract;
    property Name: String read GetName;
  end;

  TCQLName = class(TInterfacedObject, ICQLName)
  strict private
    FAlias: String;
    FCase: ICQLCase;
    FName: String;
  protected
    function GetAlias: String;
    function GetCase: ICQLCase;
    function GetName: String;
    procedure SetAlias(const Value: String);
    procedure SetCase(const Value: ICQLCase);
    procedure SetName(const Value: String);
  public
    class function New: ICQLName;
    procedure Clear;
    function IsEmpty: Boolean;
    function Serialize: String;
    property Name: String read GetName write SetName;
    property Alias: String read GetAlias write SetAlias;
    property &Case: ICQLCase read GetCase write SetCase;
  end;

  TCQLNames = class(TInterfacedObject, ICQLNames)
  private
    FColumns: TList<ICQLName>;
    function SerializeName(const AName: ICQLName): String;
    function SerializeDirection(ADirection: TOrderByDirection): String;
  protected
    function GetColumns(AIdx: Integer): ICQLName;
    constructor Create;
  public
    class function New: ICQLNames;
    destructor Destroy; override;
    function Add: ICQLName; overload; virtual;
    procedure Add(const Value: ICQLName); overload; virtual;
    procedure Clear;
    function Count: Integer;
    function IsEmpty: Boolean;
    function Serialize: String;
    property Columns[AIdx: Integer]: ICQLName read GetColumns; default;
  end;

  TCQLNameValue  = class(TInterfacedObject, ICQLNameValue)
  strict private
    FName : String;
    FValue: String;
  protected
    function GetName: String;
    function GetValue: String;
    procedure SetName(const Value: String);
    procedure SetValue(const Value: String);
  public
    procedure Clear;
    function IsEmpty: Boolean;
    property Name: String read GetName write SetName;
    property Value: String read GetValue write SetValue;
  end;

  TCQLNameValuePairs = class(TInterfacedObject, ICQLNameValuePairs)
  strict private
    FList: TList<ICQLNameValue>;
  protected
    function GetItem(AIdx: Integer): ICQLNameValue;
    constructor Create;
  public
    class function New: ICQLNameValuePairs;
    destructor Destroy; override;
    function Add: ICQLNameValue; overload;
    procedure Add(const ANameValue: ICQLNameValue); overload;
    procedure Clear;
    function Count: Integer;
    function IsEmpty: Boolean;
    property Item[AIdx: Integer]: ICQLNameValue read GetItem; default;
  end;

implementation

uses
  cqlbr.utils;

{ TCQLName }

procedure TCQLName.Clear;
begin
  FName := '';
  FAlias := '';
end;

function TCQLName.GetAlias: String;
begin
  Result := FAlias;
end;

function TCQLName.GetCase: ICQLCase;
begin
  Result := FCase;
end;

function TCQLName.GetName: String;
begin
  Result := FName;
end;

function TCQLName.IsEmpty: Boolean;
begin
  Result := (FName = '') and (FAlias = '');
end;

class function TCQLName.New: ICQLName;
begin
  Result := Self.Create;
end;

function TCQLName.Serialize: String;
begin
  if Assigned(FCase) then
    Result := '(' + FCase.Serialize + ')'
  else
    Result := FName;
  if FAlias <> '' then
    Result := Result + ' AS ' + FAlias;
end;

procedure TCQLName.SetAlias(const Value: String);
begin
  FAlias := Value;
end;

procedure TCQLName.SetCase(const Value: ICQLCase);
begin
  FCase := Value;
end;

procedure TCQLName.SetName(const Value: String);
begin
  FName := Value;
end;

{ TCQLNames }

function TCQLNames.Add: ICQLName;
begin
  Result := TCQLName.Create;
  Add(Result);
end;

procedure TCQLNames.Add(const Value: ICQLName);
begin
  FColumns.Add(Value);
end;

procedure TCQLNames.Clear;
begin
  FColumns.Clear;
end;

function TCQLNames.Count: Integer;
begin
  Result := FColumns.Count;
end;

constructor TCQLNames.Create;
begin
  FColumns := TList<ICQLName>.Create;
end;

destructor TCQLNames.Destroy;
begin
  FColumns.Free;
  inherited;
end;

function TCQLNames.GetColumns(AIdx: Integer): ICQLName;
begin
  Result := FColumns[AIdx];
end;

function TCQLNames.IsEmpty: Boolean;
begin
  Result := (Count = 0);
end;

class function TCQLNames.New: ICQLNames;
begin
  Result := Self.Create;
end;

function TCQLNames.Serialize: String;
var
  LFor: Integer;
  LOrderByCol: ICQLOrderByColumn;
begin
  Result := '';
  for LFor := 0 to FColumns.Count -1 do
  begin
    Result := TUtils.Concat([Result, SerializeName(FColumns[LFor])], ', ');
    if Supports(FColumns[LFor], ICQLOrderByColumn, LOrderByCol) then
      Result := TUtils.Concat([Result, SerializeDirection(LOrderByCol.Direction)]);
  end;
end;

function TCQLNames.SerializeDirection(ADirection: TOrderByDirection): String;
begin
  case ADirection of
    dirAscending:  Result := '';
    dirDescending: Result := 'DESC';
  else
    raise Exception.Create('TCQLNames.SerializeDirection: Unknown direction');
  end;
end;

function TCQLNames.SerializeName(const AName: ICQLName): String;
begin
  Result := AName.Serialize;
end;

{ TCQLSection }

constructor TCQLSection.Create(ASectionName: String);
begin
  FName := ASectionName;
end;

function TCQLSection.GetName: String;
begin
  Result := FName;
end;

{ TCQLNameValue }

procedure TCQLNameValue.Clear;
begin
  FName := '';
  FValue := '';
end;

function TCQLNameValue.GetName: String;
begin
  Result := FName;
end;

function TCQLNameValue.GetValue: String;
begin
  Result := FValue;
end;

function TCQLNameValue.IsEmpty: Boolean;
begin
  Result := (FName <> '');
end;

procedure TCQLNameValue.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TCQLNameValue.SetValue(const Value: String);
begin
  FValue := Value;
end;

{ TCQLNameValuePairs }

function TCQLNameValuePairs.Add: ICQLNameValue;
begin
  Result := TCQLNameValue.Create;
  Add(Result);
end;

procedure TCQLNameValuePairs.Add(const ANameValue: ICQLNameValue);
begin
  FList.Add(ANameValue);
end;

procedure TCQLNameValuePairs.Clear;
begin
  FList.Clear;
end;

function TCQLNameValuePairs.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TCQLNameValuePairs.Create;
begin
  FList := TList<ICQLNameValue>.Create;
end;

destructor TCQLNameValuePairs.Destroy;
begin
  FList.Free;
  inherited;
end;

function TCQLNameValuePairs.GetItem(AIdx: Integer): ICQLNameValue;
begin
  Result := FList[AIdx];
end;

function TCQLNameValuePairs.IsEmpty: Boolean;
begin
  Result := (Count = 0);
end;

class function TCQLNameValuePairs.New: ICQLNameValuePairs;
begin
  Result := Self.Create;
end;

end.

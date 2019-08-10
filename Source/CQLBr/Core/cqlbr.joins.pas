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

unit cqlbr.joins;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  SysUtils,
  Generics.Collections,
  cqlbr.expression,
  cqlbr.utils,
  cqlbr.core,
  cqlbr.interfaces;

type
  TCQLJoin = class(TCQLSection, ICQLJoin)
  strict private
    FCondition: ICQLExpression;
    FJoinedTable: ICQLName;
    FJoinType: TJoinType;
  protected
    function GetCondition: ICQLExpression;
    function GetJoinedTable: ICQLName;
    function GetJoinType: TJoinType;
    procedure SetCondition(const Value: ICQLExpression);
    procedure SetJoinedTable(const Value: ICQLName);
    procedure SetJoinType(const Value: TJoinType);
  public
    constructor Create;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    property Condition: ICQLExpression read GetCondition write SetCondition;
    property JoinedTable: ICQLName read GetJoinedTable write SetJoinedTable;
    property JoinType: TJoinType read GetJoinType write SetJoinType;
  end;

  TCQLJoins = class(TInterfacedObject, ICQLJoins)
  strict private
    FJoins: TList<ICQLJoin>;
    function SerializeJoinType(const AJoin: ICQLJoin): string;
  protected
    function GetJoins(AIdx: Integer): ICQLJoin;
    procedure SetJoins(AIdx: Integer; const Value: ICQLJoin);
  public
    constructor Create;
    destructor Destroy; override;
    function Add: ICQLJoin; overload;
    procedure Add(const AJoin: ICQLJoin); overload;
    procedure Clear;
    function Count: Integer;
    function IsEmpty: Boolean;
    function Serialize: String;
    property Joins[AIdx: Integer]: ICQLJoin read GetJoins write SetJoins; default;
  end;

implementation

{ TCQLJoin }

procedure TCQLJoin.Clear;
begin
  FCondition.Clear;
  FJoinedTable.Clear;
end;

constructor TCQLJoin.Create;
begin
  inherited Create('Join');
  FJoinedTable := TCQLName.New;
  FCondition := TCQLExpression.New;
end;

function TCQLJoin.GetCondition: ICQLExpression;
begin
  Result := FCondition;
end;

function TCQLJoin.GetJoinedTable: ICQLName;
begin
  Result := FJoinedTable;
end;

function TCQLJoin.GetJoinType: TJoinType;
begin
  Result := FJoinType;
end;

function TCQLJoin.IsEmpty: Boolean;
begin
  Result := (FCondition.IsEmpty and FJoinedTable.IsEmpty);
end;

procedure TCQLJoin.SetCondition(const Value: ICQLExpression);
begin
  FCondition := Value;
end;

procedure TCQLJoin.SetJoinedTable(const Value: ICQLName);
begin
  FJoinedTable := Value;
end;

procedure TCQLJoin.SetJoinType(const Value: TJoinType);
begin
  FJoinType := Value;
end;

{ TCQLJoins }

procedure TCQLJoins.Add(const AJoin: ICQLJoin);
begin
  FJoins.Add(AJoin);
end;

function TCQLJoins.Add: ICQLJoin;
begin
  Result := TCQLJoin.Create;
  Add(Result);
end;

procedure TCQLJoins.Clear;
begin
  FJoins.Clear;
end;

function TCQLJoins.Count: Integer;
begin
  Result := FJoins.Count;
end;

constructor TCQLJoins.Create;
begin
  inherited Create;
  FJoins := TList<ICQLJoin>.Create;
end;

destructor TCQLJoins.Destroy;
begin
  FJoins.Free;
  inherited;
end;

function TCQLJoins.GetJoins(AIdx: Integer): ICQLJoin;
begin
  Result := FJoins[AIdx];
end;

function TCQLJoins.IsEmpty: Boolean;
begin
  Result := (FJoins.Count = 0);
end;

function TCQLJoins.Serialize: String;
var
  LFor: Integer;
  LJoin: ICQLJoin;
begin
  Result := '';
  for LFor := 0 to Count -1 do
  begin
    LJoin := FJoins[LFor];
    Result := TUtils.Concat([Result,
                             SerializeJoinType(LJoin),
                             'JOIN',
                             LJoin.JoinedTable.Serialize,
                             'ON',
                             LJoin.Condition.Serialize]);
  end;
end;

function TCQLJoins.SerializeJoinType(const AJoin: ICQLJoin): string;
begin
  case AJoin.JoinType of
    jtINNER: Result := 'INNER';
    jtLEFT:  Result := 'LEFT';
    jtRIGHT: Result := 'RIGHT';
    jtFULL:  Result := 'FULL';
  else
    raise Exception.Create('Error Message');
  end;
end;

procedure TCQLJoins.SetJoins(AIdx: Integer; const Value: ICQLJoin);
begin
  FJoins[AIdx] := Value;
end;

end.

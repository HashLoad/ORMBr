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

unit cqlbr.ast;

interface

uses
  cqlbr.interfaces,
  cqlbr.core;

type
  TCQLAST = class(TInterfacedObject, ICQLAST)
  strict private
    FASTColumns: ICQLNames;
    FASTSection: ICQLSection;
    FASTName: ICQLName;
    FASTTableNames: ICQLNames;
    FSelect: ICQLSelect;
    FInsert: ICQLInsert;
    FUpdate: ICQLUpdate;
    FDelete : ICQLDelete;
    FGroupBy: ICQLGroupBy;
    FHaving: ICQLHaving;
    FJoins: ICQLJoins;
    FOrderBy: ICQLOrderBy;
    FWhere: ICQLWhere;
    function GetASTColumns: ICQLNames;
    procedure SetASTColumns(const Value: ICQLNames);
    function GetASTSection: ICQLSection;
    procedure SetASTSection(const Value: ICQLSection);
    function GetASTName: ICQLName;
    procedure SetASTName(const Value: ICQLName);
    function GetASTTableNames: ICQLNames;
    procedure SetASTTableNames(const Value: ICQLNames);
  protected
  public
    constructor Create(const ADatabase: TDBName);
    destructor Destroy; override;
    class function New(const ADatabase: TDBName): ICQLAST;
    procedure Clear;
    function IsEmpty: Boolean;
    function Select: ICQLSelect;
    function Delete: ICQLDelete;
    function Insert: ICQLInsert;
    function Update: ICQLUpdate;
    function Joins: ICQLJoins;
    function Where: ICQLWhere;
    function GroupBy: ICQLGroupBy;
    function Having: ICQLHaving;
    function OrderBy: ICQLOrderBy;
    property ASTColumns: ICQLNames read GetASTColumns write SetASTColumns;
    property ASTSection: ICQLSection read GetASTSection write SetASTSection;
    property ASTName: ICQLName read GetASTName write SetASTName;
    property ASTTableNames: ICQLNames read GetASTTableNames write SetASTTableNames;
  end;

implementation

uses
  cqlbr.db.register,
  cqlbr.select,
  cqlbr.orderby,
  cqlbr.where,
  cqlbr.delete,
  cqlbr.joins,
  cqlbr.groupby,
  cqlbr.having,
  cqlbr.insert,
  cqlbr.update;

{ TCQLAST }

procedure TCQLAST.Clear;
begin
  FSelect.Clear;
  FDelete.Clear;
  FInsert.Clear;
  FUpdate.Clear;
  FJoins.Clear;
  FWhere.Clear;
  FGroupBy.Clear;
  FHaving.Clear;
  FOrderBy.Clear;
end;

constructor TCQLAST.Create(const ADatabase: TDBName);
begin
  FSelect := TDBRegister.Select(ADatabase);
  if FSelect = nil then
    FSelect := TCQLSelect.Create;
  FDelete := TCQLDelete.Create;
  FInsert := TCQLInsert.Create;
  FUpdate := TCQLUpdate.Create;
  FJoins := TCQLJoins.Create;
  FWhere := TDBRegister.Where(ADatabase);
  if FWhere = nil then
    FWhere := TCQLWhere.Create;
  FGroupBy := TCQLGroupBy.Create;
  FHaving := TCQLHaving.Create;
  FOrderBy := TCQLOrderBy.Create;
end;

function TCQLAST.Delete: ICQLDelete;
begin
  Result := FDelete;
end;

destructor TCQLAST.Destroy;
begin
  inherited;
end;

function TCQLAST.GetASTColumns: ICQLNames;
begin
  Result := FASTColumns;
end;

function TCQLAST.GetASTName: ICQLName;
begin
  Result := FASTName;
end;

function TCQLAST.GetASTSection: ICQLSection;
begin
  Result := FASTSection;
end;

function TCQLAST.GetASTTableNames: ICQLNames;
begin
  Result := FASTTableNames;
end;

function TCQLAST.GroupBy: ICQLGroupBy;
begin
  Result := FGroupBy;
end;

function TCQLAST.Having: ICQLHaving;
begin
  Result := FHaving;
end;

function TCQLAST.Insert: ICQLInsert;
begin
  Result := FInsert;
end;

function TCQLAST.IsEmpty: Boolean;
begin
  Result := FSelect.IsEmpty and
            FJoins.IsEmpty and
            FWhere.IsEmpty and
            FGroupBy.IsEmpty and
            FHaving.IsEmpty and
            FOrderBy.IsEmpty;
end;

function TCQLAST.Joins: ICQLJoins;
begin
  Result := FJoins;
end;

class function TCQLAST.New(const ADatabase: TDBName): ICQLAST;
begin
  Result := Self.Create(ADatabase);
end;

function TCQLAST.OrderBy: ICQLOrderBy;
begin
  Result := FOrderBy;
end;

function TCQLAST.Select: ICQLSelect;
begin
  Result := FSelect;
end;

procedure TCQLAST.SetASTColumns(const Value: ICQLNames);
begin
  FASTColumns := Value;
end;

procedure TCQLAST.SetASTName(const Value: ICQLName);
begin
  FASTName := Value;
end;

procedure TCQLAST.SetASTSection(const Value: ICQLSection);
begin
  FASTSection := Value;
end;

procedure TCQLAST.SetASTTableNames(const Value: ICQLNames);
begin
  FASTTableNames := Value;
end;

function TCQLAST.Update: ICQLUpdate;
begin
  Result := FUpdate;
end;

function TCQLAST.Where: ICQLWhere;
begin
  Result := FWhere;
end;

end.

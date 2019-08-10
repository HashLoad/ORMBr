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

unit cqlbr.orderby;

interface

uses
  cqlbr.core,
  cqlbr.utils,
  cqlbr.interfaces;

type
  TCQLOrderByColumn = class(TCQLName, ICQLOrderByColumn)
  strict private
    FDirection: TOrderByDirection;
  protected
    function GetDirection: TOrderByDirection;
    procedure SetDirection(const Value: TOrderByDirection);
  public
    property Direction: TOrderByDirection read GetDirection write SetDirection;
  end;

  TCQLOrderByColumns = class(TCQLNames)
  public
    function Add: ICQLName; override;
  end;

  TCQLOrderBy = class(TCQLSection, ICQLOrderBy)
  strict private
    FColumns: ICQLNames;
  public
    constructor Create;
    procedure Clear; override;
    function Serialize: String;
    function IsEmpty: Boolean; override;
    function Columns: ICQLNames;
  end;

implementation

{ TCQLOrderByColumn }

function TCQLOrderByColumn.GetDirection: TOrderByDirection;
begin
  Result := FDirection;
end;

procedure TCQLOrderByColumn.SetDirection(const Value: TOrderByDirection);
begin
  FDirection := Value;
end;

{ TCQLOrderByColumns }

function TCQLOrderByColumns.Add: ICQLName;
begin
  Result := TCQLOrderByColumn.Create;
  Add(Result);
end;

{ TCQLOrderBy }

procedure TCQLOrderBy.Clear;
begin
  Columns.Clear;
end;

function TCQLOrderBy.Columns: ICQLNames;
begin
  Result := FColumns;
end;

constructor TCQLOrderBy.Create;
begin
  inherited Create('OrderBy');
  FColumns := TCQLOrderByColumns.Create;
end;

function TCQLOrderBy.IsEmpty: boolean;
begin
  Result := Columns.IsEmpty;
end;

function TCQLOrderBy.Serialize: String;
begin
  if IsEmpty then
    Result := ''
  else
    Result := TUtils.Concat(['ORDER BY', FColumns.Serialize]);
end;

end.

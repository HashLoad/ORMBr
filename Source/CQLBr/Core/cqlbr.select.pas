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

unit cqlbr.select;

interface

uses
  cqlbr.interfaces,
  cqlbr.qualifier,
  cqlbr.core;

type
  TCQLSelect = class(TCQLSection, ICQLSelect)
  protected
    FColumns: ICQLNames;
    FTableNames: ICQLNames;
    FQualifiers: ICQLSelectQualifiers;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    function Columns: ICQLNames;
    function TableNames: ICQLNames;
    function Qualifiers: ICQLSelectQualifiers;
    function Serialize: String; virtual;
  end;

implementation

uses
  cqlbr.utils,
  cqlbr.qualifier.firebird;

{ TSelect }

procedure TCQLSelect.Clear;
begin
  FColumns.Clear;
  FTableNames.Clear;
  if Assigned(FQualifiers) then
    FQualifiers.Clear;
end;

function TCQLSelect.Columns: ICQLNames;
begin
  Result := FColumns;
end;

constructor TCQLSelect.Create;
begin
  inherited Create('Select');
  FColumns := TCQLNames.New;
  FTableNames := TCQLNames.New;
end;

destructor TCQLSelect.Destroy;
begin
  inherited;
end;

function TCQLSelect.IsEmpty: Boolean;
begin
  Result := (FColumns.IsEmpty and FTableNames.IsEmpty);
end;

function TCQLSelect.Qualifiers: ICQLSelectQualifiers;
begin
  Result := FQualifiers;
end;

function TCQLSelect.Serialize: String;
begin
  if IsEmpty then
    Result := ''
  else
    Result := TUtils.Concat(['SELECT',
                             FQualifiers.SerializeDistinct,
                             FQualifiers.SerializePagination,
                             FColumns.Serialize,
                             'FROM',
                             FTableNames.Serialize]);
end;

function TCQLSelect.TableNames: ICQLNames;
begin
  Result := FTableNames;
end;

end.

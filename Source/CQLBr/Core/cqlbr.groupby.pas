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

unit cqlbr.groupby;

interface

uses
  cqlbr.core,
  cqlbr.interfaces;

type
  TCQLGroupBy = class(TCQLSection, ICQLGroupBy)
  strict private
    FColumns: ICQLNames;
  public
    constructor Create;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    function Columns: ICQLNames;
    function Serialize: String;
  end;

implementation

uses
  cqlbr.utils;

{ TGpSQLGroupBy }

constructor TCQLGroupBy.Create;
begin
  inherited Create('GroupBy');
  FColumns := TCQLNames.New;
end;

procedure TCQLGroupBy.Clear;
begin
  FColumns.Clear;
end;

function TCQLGroupBy.Columns: ICQLNames;
begin
  Result := FColumns;
end;

function TCQLGroupBy.IsEmpty: Boolean;
begin
  Result := FColumns.IsEmpty;
end;

function TCQLGroupBy.Serialize: String;
begin
  if IsEmpty then
    Result := ''
  else
    Result := TUtils.Concat(['GROUP BY', FColumns.Serialize]);
end;

end.

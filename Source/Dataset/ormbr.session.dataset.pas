{
      ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)

  ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.session.dataset;

interface

uses
  DB,
  Rtti,
  TypInfo,
  Classes,
  Variants,
  SysUtils,
  Generics.Collections,
  /// orm
  ormbr.command.executor,
  ormbr.session.abstract,
  ormbr.dataset.base.adapter,
  dbcbr.mapping.classes,
  dbebr.factory.interfaces;

type
  // M - Sess�o DataSet
  TSessionDataSet<M: class, constructor> = class(TSessionAbstract<M>)
  private
    FOwner: TDataSetBaseAdapter<M>;
    procedure PopularDataSet(const ADBResultSet: IDBResultSet);
  protected
    FConnection: IDBConnection;
  public
    constructor Create(const AOwner: TDataSetBaseAdapter<M>;
      const AConnection: IDBConnection; const APageSize: Integer = -1); overload;
    destructor Destroy; override;
    procedure OpenID(const AID: Variant); override;
    procedure OpenSQL(const ASQL: string); override;
    procedure OpenWhere(const AWhere: string; const AOrderBy: string = ''); override;
    procedure NextPacket; override;
    procedure RefreshRecord(const AColumns: TParams); override;
    procedure RefreshRecordWhere(const AWhere: String); override;
    function SelectAssociation(const AObject: TObject): String; override;
  end;

implementation

uses
  ormbr.bind;

{ TSessionDataSet<M> }

constructor TSessionDataSet<M>.Create(const AOwner: TDataSetBaseAdapter<M>;
  const AConnection: IDBConnection; const APageSize: Integer);
begin
  inherited Create(APageSize);
  FOwner := AOwner;
  FConnection := AConnection;
  FCommandExecutor := TSQLCommandExecutor<M>.Create(Self, AConnection, APageSize);
end;

destructor TSessionDataSet<M>.Destroy;
begin
  FCommandExecutor.Free;
  inherited;
end;

function TSessionDataSet<M>.SelectAssociation(const AObject: TObject): String;
begin
  inherited;
  Result := FCommandExecutor.SelectInternalAssociation(AObject);
end;

procedure TSessionDataSet<M>.OpenID(const AID: Variant);
var
  LDBResultSet: IDBResultSet;
begin
  inherited;
  LDBResultSet := FCommandExecutor.SelectInternalID(AID);
  PopularDataSet(LDBResultSet);
end;

procedure TSessionDataSet<M>.OpenSQL(const ASQL: string);
var
  LDBResultSet: IDBResultSet;
begin
  inherited;
  if ASQL = '' then
    LDBResultSet := FCommandExecutor.SelectInternalAll
  else
    LDBResultSet := FCommandExecutor.SelectInternal(ASQL);
  PopularDataSet(LDBResultSet);
end;

procedure TSessionDataSet<M>.OpenWhere(const AWhere: string;
  const AOrderBy: string);
begin
  inherited;
  OpenSQL(FCommandExecutor.SelectInternalWhere(AWhere, AOrderBy));
end;

procedure TSessionDataSet<M>.RefreshRecord(const AColumns: TParams);
var
  LDBResultSet: IDBResultSet;
  LWhere: String;
  LFor: Integer;
begin
  inherited;
  LWhere := '';
  for LFor := 0 to AColumns.Count -1 do
  begin
    LWhere := LWhere + AColumns[LFor].Name + '=' + AColumns[LFor].AsString;
    if LFor < AColumns.Count -1 then
      LWhere := LWhere + ' AND ';
  end;
  LDBResultSet := FCommandExecutor.SelectInternal(FCommandExecutor.SelectInternalWhere(LWhere, ''));
  while LDBResultSet.NotEof do
  begin
    FOwner.FOrmDataSet.Edit;
    Bind.SetFieldToField(LDBResultSet, FOwner.FOrmDataSet);
    FOwner.FOrmDataSet.Post;
  end;
end;

procedure TSessionDataSet<M>.RefreshRecordWhere(const AWhere: String);
var
  LDBResultSet: IDBResultSet;
begin
  inherited;
  LDBResultSet := FCommandExecutor.SelectInternal(FCommandExecutor.SelectInternalWhere(AWhere, ''));
  while LDBResultSet.NotEof do
  begin
    FOwner.FOrmDataSet.Edit;
    Bind.SetFieldToField(LDBResultSet, FOwner.FOrmDataSet);
    FOwner.FOrmDataSet.Post;
  end;
end;

procedure TSessionDataSet<M>.NextPacket;
var
  LDBResultSet: IDBResultSet;
begin
  inherited;
  LDBResultSet := FCommandExecutor.NextPacket;
  if LDBResultSet.RecordCount > 0 then
    PopularDataSet(LDBResultSet)
  else
    FFetchingRecords := True;
end;

procedure TSessionDataSet<M>.PopularDataSet(const ADBResultSet: IDBResultSet);
begin
//  FOrmDataSet.Locate(KeyFiels, KeyValues, Options);
//  { TODO -oISAQUE : Procurar forma de verificar se o registro n�o j� est� em mem�ria
//  pela chave primaria }
  try
    while ADBResultSet.NotEof do
    begin
       FOwner.FOrmDataSet.Append;
       Bind.SetFieldToField(ADBResultSet, FOwner.FOrmDataSet);
       FOwner.FOrmDataSet.Fields[0].AsInteger := -1;
       FOwner.FOrmDataSet.Post;
    end;
  finally
    ADBResultSet.Close;
  end;
end;

end.

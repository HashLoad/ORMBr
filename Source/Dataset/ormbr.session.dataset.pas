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
  ormbr.mapping.classes,
  ormbr.mapping.explorerstrategy,
  ormbr.objects.manager,
  ormbr.objects.manager.abstract,
  ormbr.session.abstract,
  ormbr.dataset.base.adapter,
  ormbr.factory.interfaces;

type
  /// <summary>
  /// M - Sessão DataSet
  /// </summary>
  TSessionDataSet<M: class, constructor> = class(TSessionAbstract<M>)
  private
    FOwner: TDataSetBaseAdapter<M>;
    procedure PopularDataSet(const ADBResultSet: IDBResultSet);
  protected
    FConnection: IDBConnection;
    procedure OpenSQL(const ASQL: string); override;
  public
    constructor Create(const AOwner: TDataSetBaseAdapter<M>;
      const AConnection: IDBConnection; const APageSize: Integer = -1); overload;
    destructor Destroy; override;
    procedure OpenID(const AID: Variant); override;
    procedure OpenWhere(const AWhere: string; const AOrderBy: string = ''); override;
//    procedure OpenAssociation(const AObject: TObject); override;
    procedure NextPacket; override;
    procedure RefreshRecord(const AColumnName: string); override;
  end;

implementation

uses
  ormbr.dataset.bind;

{ TSessionDataSet<M> }

constructor TSessionDataSet<M>.Create(const AOwner: TDataSetBaseAdapter<M>;
  const AConnection: IDBConnection; const APageSize: Integer);
begin
  inherited Create(APageSize);
  FOwner := AOwner;
  FConnection := AConnection;
  FManager := TObjectManager<M>.Create(Self, AConnection, APageSize);
end;

destructor TSessionDataSet<M>.Destroy;
begin
  FManager.Free;
  inherited;
end;

//procedure TSessionDataSet<M>.OpenAssociation(const AObject: TObject);
//var
//  LDBResultSet: IDBResultSet;
//begin
//  inherited;
//  LDBResultSet := FManager.SelectInternalAssociation(AObject);
//  /// <summary>
//  /// Popula o DataSet em memória com os registros retornardos no comando SQL
//  /// </summary>
//  PopularDataSet(LDBResultSet);
//end;

procedure TSessionDataSet<M>.OpenID(const AID: Variant);
var
  LDBResultSet: IDBResultSet;
begin
  FManager.FetchingRecords := False;
  LDBResultSet := FManager.SelectInternalID(AID);
  /// <summary>
  /// Popula o DataSet em memória com os registros retornardos no comando SQL
  /// </summary>
  PopularDataSet(LDBResultSet);
end;

procedure TSessionDataSet<M>.OpenSQL(const ASQL: string);
var
  LDBResultSet: IDBResultSet;
begin
  FManager.FetchingRecords := False;
  if ASQL = '' then
    LDBResultSet := FManager.SelectInternalAll
  else
    LDBResultSet := FManager.SelectInternal(ASQL);
  /// <summary>
  /// Popula o DataSet em memória com os registros retornardos no comando SQL
  /// </summary>
  PopularDataSet(LDBResultSet);
end;

procedure TSessionDataSet<M>.OpenWhere(const AWhere: string;
  const AOrderBy: string);
begin
  OpenSQL(FManager.SelectInternalWhere(AWhere, AOrderBy));
end;

procedure TSessionDataSet<M>.RefreshRecord(const AColumnName: string);
var
  LDBResultSet: IDBResultSet;
begin
  inherited;
  LDBResultSet := FManager
                    .SelectInternalID(FOwner.FOrmDataSet.FieldByName(AColumnName).AsInteger);
  /// Atualiza dados no DataSet
  while LDBResultSet.NotEof do
  begin
    FOwner.FOrmDataSet.Edit;
    TBindDataSet
      .GetInstance
        .SetFieldToField(LDBResultSet, FOwner.FOrmDataSet);
    FOwner.FOrmDataSet.Post;
  end;
end;

procedure TSessionDataSet<M>.NextPacket;
var
  LDBResultSet: IDBResultSet;
begin
  if not FManager.FetchingRecords then
  begin
    LDBResultSet := FManager.NextPacket;
    /// <summary>
    /// Popula o DataSet em memória com os registros retornardos no comando SQL
    /// </summary>
    PopularDataSet(LDBResultSet);
  end;
end;

procedure TSessionDataSet<M>.PopularDataSet(const ADBResultSet: IDBResultSet);
begin
//  FOrmDataSet.Locate(KeyFiels, KeyValues, Options);
//  { TODO -oISAQUE : Procurar forma de verificar se o registro não já está em memória
//  pela chave primaria }
  while ADBResultSet.NotEof do
  begin
     FOwner.FOrmDataSet.Append;
     TBindDataSet
       .GetInstance
         .SetFieldToField(ADBResultSet, FOwner.FOrmDataSet);
     FOwner.FOrmDataSet.Fields[0].AsInteger := -1;
     FOwner.FOrmDataSet.Post;
  end;
  ADBResultSet.Close;
end;

end.

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
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)

  ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.dataset.adapter;

interface

uses
  DB,
  Rtti,
  TypInfo,
  Classes,
  SysUtils,
  StrUtils,
  Variants,
  Generics.Collections,
  /// orm
  ormbr.criteria,
  ormbr.dataset.bind,
  ormbr.mapping.classes,
  ormbr.types.mapping,
  ormbr.session.dataset,
  ormbr.factory.interfaces,
  ormbr.dataset.base.adapter;

type
  TDataSetHack = class(TDataSet)
  end;
  /// <summary>
  /// M - Object M
  /// </summary>
  TDataSetAdapter<M: class, constructor> = class(TDataSetBaseAdapter<M>)
  private
    function GetRelationFields(ATable: TTableMapping;
      ADetail: TDataSetBaseAdapter<M>; var ACriteria: ICriteria): Boolean;
    procedure ExecuteCheckNotNull;
  protected
    FConnection: IDBConnection;
    procedure OpenDataSetChilds; override;
    procedure RefreshDataSetOneToOneChilds(AFieldName: string); override;
    procedure DoAfterScroll(DataSet: TDataSet); override;
    procedure DoBeforePost(DataSet: TDataSet); override;
    procedure DoBeforeDelete(DataSet: TDataSet); override;
    procedure DoNewRecord(DataSet: TDataSet); override;
    procedure LoadLazy(const AOwner: M); override;
    procedure NextPacket; override;
  public
    constructor Create(AConnection: IDBConnection; ADataSet:
      TDataSet; APageSize: Integer; AMasterObject: TObject); overload;
    destructor Destroy; override;
  end;

implementation

uses
  ormbr.mapping.explorer,
  ormbr.objects.helper,
  ormbr.rtti.helper,
  ormbr.dataset.fields,
  ormbr.objectset.bind,
  ormbr.mapping.exceptions;

{ TDataSetAdapter<M> }

constructor TDataSetAdapter<M>.Create(AConnection: IDBConnection;
  ADataSet: TDataSet; APageSize: Integer; AMasterObject: TObject);
begin
  FConnection := AConnection;
  inherited Create(ADataSet, APageSize, AMasterObject);
  /// <summary>
  /// Session que ser� usado pelo Adapter
  /// </summary>
  FSession := TSessionDataSet<M>.Create(Self, AConnection, APageSize);
end;

destructor TDataSetAdapter<M>.Destroy;
begin
  FSession.Free;
  inherited;
end;

procedure TDataSetAdapter<M>.DoAfterScroll(DataSet: TDataSet);
begin
  if DataSet.State in [dsBrowse] then
  begin
//    /// <summary>
//    ///  Alternativa para abertura de sub-tabelas do atributo "Association",
//    ///  o m�todo atual habilitado tem melhor peformance
//    /// </summary>
//    TBindDataSet.GetInstance.SetFieldToProperty(FOrmDataSet, TObject(FCurrentInternal));
    OpenDataSetChilds;
  end;
  inherited;
end;

procedure TDataSetAdapter<M>.DoBeforeDelete(DataSet: TDataSet);
var
  LDataSet: TDataSet;
  LFor: Integer;
begin
  inherited DoBeforeDelete(DataSet);
  /// <summary> Alimenta a lista com registros deletados </summary>
  FSession.DeleteList.Add(M.Create);
  TBindObject
    .GetInstance
      .SetFieldToProperty(FOrmDataSet, TObject(FSession.DeleteList.Last));

  /// <summary> Deleta registros de todos os DataSet filhos </summary>
  EmptyDataSetChilds;
  /// <summary>
  /// Exclui os registros dos NestedDataSets linkados ao FOrmDataSet
  /// Recurso usado em banco NoSQL
  /// </summary>
  for LFor := 0 to TDataSetHack(FOrmDataSet).NestedDataSets.Count - 1 do
  begin
    LDataSet := TDataSetHack(FOrmDataSet).NestedDataSets.Items[LFor];
    LDataSet.DisableControls;
    LDataSet.First;
    try
      repeat
        LDataSet.Delete;
      until LDataSet.Eof;
    finally
      LDataSet.EnableControls;
    end;
  end;
end;

procedure TDataSetAdapter<M>.DoBeforePost(DataSet: TDataSet);
begin
  inherited DoBeforePost(DataSet);
  /// <summary>
  /// Rotina de valida��o se o campo foi deixado null
  /// </summary>
  ExecuteCheckNotNull;
end;

procedure TDataSetAdapter<M>.ExecuteCheckNotNull;
var
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
begin
  LColumns := TMappingExplorer
                .GetInstance
                  .GetMappingColumn(FCurrentInternal.ClassType);
  for LColumn in LColumns do
  begin
    if LColumn.IsNoInsert then
      Continue;
    if LColumn.IsNoUpdate then
      Continue;
    if LColumn.IsJoinColumn then
      Continue;
    if LColumn.IsNoValidate then
      Continue;
    if LColumn.PropertyRtti.IsNullable then
      Continue;
    if LColumn.FieldType in [ftDataSet, ftADT] then
      Continue;

    if FOrmDataSet.FieldValues[LColumn.ColumnName] = Null then
      raise EFieldValidate.Create(FCurrentInternal.ClassName + '.' + LColumn.ColumnName,
                                  FOrmDataSet.FieldByName(LColumn.ColumnName).ConstraintErrorMessage);
  end;
end;

procedure TDataSetAdapter<M>.OpenDataSetChilds;
var
  LTable: TTableMapping;
  LDataSetChild: TDataSetBaseAdapter<M>;
  LCriteria: ICriteria;
begin
  inherited;
  if FOrmDataSet.Active then
  begin
    if FOrmDataSet.RecordCount > 0 then
    begin
      /// <summary>
      /// Se Count > 0, identifica-se que � o objeto � o Master
      /// </summary>
      if FMasterObject.Count > 0 then
      begin
        for LDataSetChild in FMasterObject.Values do
        begin
//          /// <summary>
//          ///  Alternativa para abertura de sub-tabelas, o m�todo atual
//          ///  habilitado tem melhor peformance
//          /// </summary>
//          LDataSetChild.FOrmDataSet.DisableControls;
//          LDataSetChild.DisableDataSetEvents;
//          LDataSetChild.EmptyDataSet;
//          try
//            LDataSetChild.FSession.OpenAssociation(FCurrentInternal);
//          finally
//            LDataSetChild.FOrmDataSet.EnableControls;
//            LDataSetChild.EnableDataSetEvents;
//          end;
          LTable := FExplorer
                      .GetMappingTable(LDataSetChild.FCurrentInternal.ClassType);
          if LTable <> nil then
          begin
            LCriteria := CreateCriteria.Select;
            /// <summary>
            /// Gera o comando SQL do SELECT para abertura da tabela associada
            /// </summary>
            if GetRelationFields(LTable, LDataSetChild, LCriteria) then
              LDataSetChild.OpenSQLInternal(LCriteria.AsString);
          end;
        end;
      end;
    end;
  end;
end;

function TDataSetAdapter<M>.GetRelationFields(ATable: TTableMapping;
  ADetail: TDataSetBaseAdapter<M>; var ACriteria: ICriteria): Boolean;
var
  LAssociations: TAssociationMappingList;
  LAssociation: TAssociationMapping;
  LFor: Integer;
  LValue: String;
begin
  Result := False;
  LAssociations := FExplorer.GetMappingAssociation(FCurrentInternal.ClassType);
  if LAssociations <> nil then
  begin
    for LAssociation in LAssociations do
    begin
      /// <summary>
      /// Verifica��o se tem algum mapeamento OneToOne para a classe.
      /// </summary>
      if LAssociation.ClassNameRef = ADetail.FCurrentInternal.ClassName then
      begin
        /// <summary>
        /// O FROM pelo nome da classe de referencia
        /// O WHERE pela coluna de referencia.
        /// </summary>
        for LFor := 0 to LAssociation.ColumnsNameRef.Count -1 do
        begin
          LValue := TBindDataSet.GetInstance
                      .GetFieldValue(FOrmDataSet,
                                     LAssociation.ColumnsName[LFor],
                                     FOrmDataSet.FieldByName(LAssociation.ColumnsName[LFor]).DataType);
          if Length(LValue) > 0 then
          begin
            ACriteria
              .All
                .From(ATable.Name)
                  .Where(ATable.Name + '.' + LAssociation.ColumnsNameRef[LFor] + '=' + LValue);
          end;
        end;
        if Length(ACriteria.AsString) > 0 then
          Result := True;
      end;
    end;
  end;
end;

procedure TDataSetAdapter<M>.LoadLazy(const AOwner: M);
var
  LTable: TTableMapping;
  LCriteria: ICriteria;
begin
  inherited;
  if AOwner <> nil then
  begin
    if FOwnerMasterObject = nil then
    begin
      if not FOrmDataSet.Active then
      begin
        SetMasterObject(AOwner);
//        /// <summary>
//        ///  Alternativa para abertura de sub-tabelas, o m�todo atual
//        ///  habilitado tem melhor peformance
//        /// </summary>
//        FOrmDataSet.DisableControls;
//        DisableDataSetEvents;
//        EmptyDataSet;
//        try
//          FSession.OpenAssociation(FCurrentInternal);
//        finally
//          FOrmDataSet.EnableControls;
//          EnableDataSetEvents;
//        end;
        LTable := FExplorer.GetMappingTable(FCurrentInternal.ClassType);
        if LTable <> nil then
        begin
          LCriteria := CreateCriteria.Select;
          /// <summary>
          /// Gera o comando SQL do SELECT para abertura da tabela associada
          /// </summary>
          GetRelationFields(LTable, Self, LCriteria);
          OpenSQLInternal(LCriteria.AsString);
        end;
      end;
    end;
  end
  else
  begin
    if FOwnerMasterObject <> nil then
    begin
      if TDataSetBaseAdapter<M>(FOwnerMasterObject).FOrmDataSet.Active then
      begin
        SetMasterObject(nil);
        Close;
      end;
    end
  end;
end;

procedure TDataSetAdapter<M>.NextPacket;
var
  LBookMark: TBookmark;
begin
  inherited;
  FOrmDataSet.DisableControls;
  DisableDataSetEvents;
  LBookMark := FOrmDataSet.Bookmark;
  try
    FSession.NextPacket;
  finally
    FOrmDataSet.GotoBookmark(LBookMark);
    FOrmDataSet.EnableControls;
    EnableDataSetEvents;
  end;
end;

procedure TDataSetAdapter<M>.RefreshDataSetOneToOneChilds(AFieldName: string);
var
  LTable: TTableMapping;
  LAssociations: TAssociationMappingList;
  LAssociation: TAssociationMapping;
  LDataSetChild: TDataSetBaseAdapter<M>;
  LCriteria: ICriteria;
begin
  inherited;
  if FOrmDataSet.Active then
  begin
    LAssociations := FExplorer
                       .GetMappingAssociation(FCurrentInternal.ClassType);
    if LAssociations <> nil then
    begin
      for LAssociation in LAssociations do
      begin
        if LAssociation.Multiplicity in [OneToOne, ManyToOne] then
        begin
          /// <summary>
          /// Checa se o campo que recebeu a altera��o, � um campo de associa��o
          /// Se for � feito um novo select para atualizar a propriedade associada.
          /// </summary>
          if LAssociation.ColumnsName.IndexOf(AFieldName) > -1 then
          begin
            if FMasterObject.ContainsKey(LAssociation.ClassNameRef) then
            begin
              LDataSetChild := FMasterObject.Items[LAssociation.ClassNameRef];
              if LDataSetChild <> nil then
              begin
//                /// <summary>
//                ///  Alternativa para abertura de sub-tabelas do atributo "Association",
//                /// o m�todo atual habilitado tem melhor peformance
//                /// </summary>
//                LDataSetChild.FOrmDataSet.DisableControls;
//                LDataSetChild.DisableDataSetEvents;
//                LDataSetChild.EmptyDataSet;
//                try
//                  LDataSetChild.FSession.OpenAssociation(FCurrentInternal);
//                finally
//                  LDataSetChild.FOrmDataSet.EnableControls;
//                  LDataSetChild.EnableDataSetEvents;
//                end;
                LTable := FExplorer
                            .GetMappingTable(LDataSetChild.FCurrentInternal.ClassType);
                if LTable <> nil then
                begin
                  LCriteria := CreateCriteria.Select;
                  /// <summary>
                  /// Gera o comando SQL do SELECT para abertura da tabela associada
                  /// </summary>
                  GetRelationFields(LTable, LDataSetChild, LCriteria);
                  LDataSetChild.OpenSQLInternal(LCriteria.AsString);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TDataSetAdapter<M>.DoNewRecord(DataSet: TDataSet);
begin
  /// <summary>
  /// Limpa registros do dataset em mem�ria antes de receber os novos registros
  /// </summary>
  EmptyDataSetChilds;
  inherited DoNewRecord(DataSet);
end;

end.

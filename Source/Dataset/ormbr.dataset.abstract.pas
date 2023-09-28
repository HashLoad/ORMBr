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
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)

  ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi.
}

unit ormbr.dataset.abstract;

interface

uses
  DB,
  Rtti,
  Generics.Collections,
  ormbr.dataset.fields,
  ormbr.session.abstract,
  dbcbr.mapping.classes,
  dbcbr.rtti.helper;

type
  TDataSetLocal = class(TDataSet)
  protected
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: Boolean; override;
  end;

  TDataSetAbstract<M: class, constructor> = class abstract
  protected
    FSession: TSessionAbstract<M>;
    FOrmDataSource: TDataSource;
    procedure RefreshDataSetOneToOneChilds(AFieldName: string); virtual;
    procedure DoDataChange(Sender: TObject; Field: TField); virtual;
    // Abstract
    procedure DoBeforeApplyUpdates(DataSet: TDataSet); overload; virtual; abstract;
    procedure DoAfterApplyUpdates(DataSet: TDataSet; AErrors: Integer); overload; virtual; abstract;
    procedure DoBeforeApplyUpdates(Sender: TObject; var OwnerData: OleVariant); overload; virtual; abstract;
    procedure DoAfterApplyUpdates(Sender: TObject; var OwnerData: OleVariant); overload; virtual; abstract;
    procedure OpenIDInternal(const AID: TValue); overload; virtual; abstract;
    procedure ApplyInserter(const MaxErros: Integer); virtual; abstract;
    procedure ApplyUpdater(const MaxErros: Integer); virtual; abstract;
    procedure ApplyDeleter(const MaxErros: Integer); virtual; abstract;
    procedure ApplyInternal(const MaxErros: Integer); virtual; abstract;
    procedure LoadLazy(const AOwner: M); virtual; abstract;
    procedure OpenDataSetChilds; virtual; abstract;
    procedure EmptyDataSetChilds; virtual; abstract;
    procedure OpenSQLInternal(const ASQL: string); virtual; abstract;
    procedure OpenWhereInternal(const AWhere: string; const AOrderBy: string = ''); virtual; abstract;
    procedure ApplyUpdates(const MaxErros: Integer); virtual; abstract;
    procedure EmptyDataSet; virtual; abstract;
  public
    FOrmDataSet: TDataSet;
    constructor Create(ADataSet: TDataSet; APageSize: Integer;
      AMasterObject: TObject); overload; virtual;
    destructor Destroy; override;
  end;

implementation

uses
  dbcbr.mapping.explorer;

{ TDataSetAbstract<M> }

constructor TDataSetAbstract<M>.Create(ADataSet: TDataSet; APageSize: Integer;
  AMasterObject: TObject);
begin
  FOrmDataSource := TDataSource.Create(nil);
  FOrmDataSource.DataSet := FOrmDataSet;
  FOrmDataSource.OnDataChange := DoDataChange;
end;

destructor TDataSetAbstract<M>.Destroy;
begin
  FOrmDataSource.Free;
  inherited;
end;

procedure TDataSetAbstract<M>.DoDataChange(Sender: TObject; Field: TField);
var
  LValues: TDictionary<string, string>;
//  LContext: TRttiContext;
//  LObjectType: TRttiType;
  LColumn: TColumnMapping;
  LColumns: TColumnMappingList;
begin
  if not (FOrmDataSet.State in [dsInsert, dsEdit]) then
    Exit;
  if Field = nil then
    Exit;
  if Field.Tag > 0 then
    Exit;
  if (Field.FieldKind <> fkData) or (Field.FieldName = cInternalField) then
    Exit;
  // Só adiciona a lista se for edição
  if FOrmDataSet.State in [dsEdit] then
  begin
    LValues := FSession.ModifiedFields.Items[M.ClassName];
    if LValues <> nil then
    begin
      if not LValues.ContainsValue(Field.FieldName) then
      begin
        LColumns := TMappingExplorer.GetMappingColumn(M);
        for LColumn in LColumns do
        begin
          if LColumn.ColumnProperty = nil then
            Continue;
          if LColumn.ColumnProperty.IsVirtualData then
            Continue;
          if LColumn.ColumnProperty.IsNoUpdate then
            Continue;
          if LColumn.ColumnProperty.IsAssociation then
            Continue;
          if LColumn.ColumnName <> Field.FieldName then
            Continue;
          LValues.Add(LColumn.ColumnProperty.Name, Field.FieldName);
          Break;
        end;
//        LObjectType := LContext.GetType(TypeInfo(M));
//        for LProperty in LObjectType.GetProperties do
//        begin
//          if LProperty.GetColumn.ColumnName = Field.FieldName then
//          begin
//            LValue.Add(LProperty.Name, Field.FieldName);
//            Break;
//          end;
//        end;
      end;
    end;
  end;
  // Atualiza o registro da tabela externa, se o campo alterado
  // pertencer a um relacionamento OneToOne ou ManyToOne
  RefreshDataSetOneToOneChilds(Field.FieldName);
end;

procedure TDataSetAbstract<M>.RefreshDataSetOneToOneChilds(AFieldName: string);
begin

end;

{ TDataSetLocal }

procedure TDataSetLocal.InternalClose;
begin
  inherited;

end;

procedure TDataSetLocal.InternalHandleException;
begin
  inherited;

end;

procedure TDataSetLocal.InternalInitFieldDefs;
begin
  inherited;

end;

procedure TDataSetLocal.InternalOpen;
begin
  inherited;

end;

function TDataSetLocal.IsCursorOpen: Boolean;
begin
  Result := false;
end;

end.

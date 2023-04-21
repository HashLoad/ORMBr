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
  // M - Object M
  TDataSetAbstract<M: class, constructor> = class abstract
  protected
    FSession: TSessionAbstract<M>;
    // Objeto para controle de estado do registro
    FOrmDataSource: TDataSource;
    procedure RefreshDataSetOneToOneChilds(AFieldName: string); virtual;
    procedure DoDataChange(Sender: TObject; Field: TField); virtual;
  public
    // Objeto interface com o DataSet passado pela interface.
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
  // S� adiciona a lista se for edi��o
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

end.

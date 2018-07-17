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
  Generics.Collections,
  ormbr.dataset.fields,
  ormbr.session.abstract;

type
  /// <summary>
  /// M - Object M
  /// </summary>
  TDataSetAbstract<M: class, constructor> = class abstract
  protected
    FSession: TSessionAbstract<M>;
    /// <summary>
    /// Objeto para controle de estado do registro
    /// </summary>
    FOrmDataSource: TDataSource;
    procedure RefreshDataSetOneToOneChilds(AFieldName: string); virtual;
    procedure DoDataChange(Sender: TObject; Field: TField); virtual;
  public
    /// <summary>
    /// Objeto interface com o DataSet passado pela interface.
    /// </summary>
    FOrmDataSet: TDataSet;
    constructor Create(ADataSet: TDataSet; APageSize: Integer;
      AMasterObject: TObject); overload; virtual;
    destructor Destroy; override;
  end;

implementation

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
begin
  if FOrmDataSet.State in [dsInsert, dsEdit] then
  begin
    if Field <> nil then
    begin
      if Field.FieldKind = fkData then
      begin
        if Field.FieldName <> cInternalField then
        begin
          /// <summary> Só adiciona a lista se for edição </summary>
          if FOrmDataSet.State in [dsEdit] then
          begin
            with FSession.ModifiedFields.Items[M.ClassName] do
            begin
              if IndexOf(Field.FieldName) = -1 then
                Add(Field.FieldName);
            end;
          end;
          /// <summary>
          /// Atualiza o registro da tabela externa, se o campo alterado
          /// pertencer a um relacionamento OneToOne ou ManyToOne
          /// </summary>
          RefreshDataSetOneToOneChilds(Field.FieldName);
        end;
      end;
    end;
  end;
end;

procedure TDataSetAbstract<M>.RefreshDataSetOneToOneChilds(AFieldName: string);
begin

end;

end.

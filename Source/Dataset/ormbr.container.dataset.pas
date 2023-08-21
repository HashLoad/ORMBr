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

unit ormbr.container.dataset;

interface

uses
  DB,
  RTTi,
  Classes,
  SysUtils,
  Generics.Collections,
  /// ormbr
  ormbr.container.dataset.interfaces,
  ormbr.session.abstract,
  dbebr.factory.interfaces,
  ormbr.dataset.base.adapter;

type
  TContainerDataSet<M: class, constructor> = class(TInterfacedObject, IContainerDataSet<M>)
  private
    function _GetAutoNextPacket: Boolean;
    procedure _SetAutoNextPacket(const Value: Boolean);
  protected
    FDataSetAdapter: TDataSetBaseAdapter<M>;
  public
    destructor Destroy; override;
    procedure LoadLazy(AOwner: M);
    procedure Open; overload;
    procedure Open(const AID: Integer); overload;
    procedure Open(const AID: String); overload;
    procedure OpenWhere(const AWhere: string; const AOrderBy: string = '');
    procedure OpenSQL(const ASQL: String);
    procedure Insert;
    procedure Append;
    procedure Post;
    procedure Edit;
    procedure Delete;
    procedure Close;
    procedure Cancel;
    procedure RefreshRecord;
    procedure RefreshRecordWhere(const AWhere: string);
    procedure EmptyDataSet;
    procedure CancelUpdates;
    procedure Save(AObject: M);
    procedure ApplyUpdates(MaxErros: Integer);
    procedure AddLookupField(AFieldName: string;
                             AKeyFields: string;
                             ALookupDataSet: TObject;
                             ALookupKeyFields: string;
                             ALookupResultField: string;
                             ADisplayLabel: string = '');
    procedure NextPacket; virtual;
    function DataSet: TDataSet;
    function MasterObject: TDataSetBaseAdapter<M>;
    function This: TDataSetBaseAdapter<M>;
    function Current: M;
    // ObjectSet
    function Find: TObjectList<M>; overload;
    function Find(const AID: Integer): M; overload;
    function Find(const AID: String): M; overload;
    function FindWhere(const AWhere: string; const AOrderBy: string = ''): TObjectList<M>;
    //
    property AutoNextPacket: Boolean read _GetAutoNextPacket write _SetAutoNextPacket;
  end;

implementation

{ TContainerDataSet<M> }

procedure TContainerDataSet<M>.AddLookupField(AFieldName, AKeyFields: string;
  ALookupDataSet: TObject; ALookupKeyFields, ALookupResultField: string;
  ADisplayLabel: string);
begin
  inherited;
  FDataSetAdapter.AddLookupField(AFieldName,
                                 AKeyFields,
                                 ALookupDataSet,
                                 ALookupKeyFields,
                                 ALookupResultField,
                                 ADisplayLabel);
end;

procedure TContainerDataSet<M>.Append;
begin
  FDataSetAdapter.Append;
end;

procedure TContainerDataSet<M>.ApplyUpdates(MaxErros: Integer);
begin
  FDataSetAdapter.ApplyUpdates(MaxErros);
end;

procedure TContainerDataSet<M>.Cancel;
begin
  FDataSetAdapter.Cancel;
end;

procedure TContainerDataSet<M>.CancelUpdates;
begin
  FDataSetAdapter.CancelUpdates;
end;

procedure TContainerDataSet<M>.Close;
begin
  FDataSetAdapter.EmptyDataSet;
end;

function TContainerDataSet<M>.MasterObject: TDataSetBaseAdapter<M>;
begin
  Result := FDataSetAdapter;
end;

procedure TContainerDataSet<M>.Delete;
begin
  FDataSetAdapter.Delete;
end;

destructor TContainerDataSet<M>.Destroy;
begin
  inherited;
end;

procedure TContainerDataSet<M>.Edit;
begin
  FDataSetAdapter.Edit;
end;

procedure TContainerDataSet<M>.EmptyDataSet;
begin
  FDataSetAdapter.EmptyDataSet;
end;

function TContainerDataSet<M>.Find: TObjectList<M>;
begin
  Result := FDataSetAdapter.Find;
end;

function TContainerDataSet<M>.Find(const AID: Integer): M;
begin
  Result := FDataSetAdapter.Find(AID);
end;

function TContainerDataSet<M>.Find(const AID: String): M;
begin
  Result := FDataSetAdapter.Find(AID);
end;

function TContainerDataSet<M>.FindWhere(const AWhere, AOrderBy: string): TObjectList<M>;
begin
  Result := FDataSetAdapter.FindWhere(AWhere, AOrderBy);
end;

function TContainerDataSet<M>._GetAutoNextPacket: Boolean;
begin
  Result := FDataSetAdapter.AutoNextPacket
end;

function TContainerDataSet<M>.Current: M;
begin
  Result := FDataSetAdapter.Current;
end;

function TContainerDataSet<M>.DataSet: TDataSet;
begin
  Result := FDataSetAdapter.FOrmDataSet;
end;

procedure TContainerDataSet<M>.Insert;
begin
  inherited;
  FDataSetAdapter.Insert;
end;

procedure TContainerDataSet<M>.LoadLazy(AOwner: M);
begin
  FDataSetAdapter.LoadLazy(AOwner);
end;

procedure TContainerDataSet<M>.NextPacket;
begin
  FDataSetAdapter.NextPacket;
end;

procedure TContainerDataSet<M>.Open;
begin
  FDataSetAdapter.OpenSQLInternal('');
end;

procedure TContainerDataSet<M>.OpenWhere(const AWhere, AOrderBy: string);
begin
  FDataSetAdapter.OpenWhereInternal(AWhere, AOrderBy);
end;

procedure TContainerDataSet<M>.Open(const AID: String);
begin
  FDataSetAdapter.OpenIDInternal(AID);
end;

procedure TContainerDataSet<M>.OpenSQL(const ASQL: String);
begin
  FDataSetAdapter.OpenSQLInternal(ASQL);
end;

procedure TContainerDataSet<M>.Open(const AID: Integer);
begin
  FDataSetAdapter.OpenIDInternal(AID);
end;

procedure TContainerDataSet<M>.Post;
begin
  FDataSetAdapter.Post;
end;

procedure TContainerDataSet<M>.RefreshRecord;
begin
  FDataSetAdapter.RefreshRecord;
end;

procedure TContainerDataSet<M>.RefreshRecordWhere(const AWhere: string);
begin
  FDataSetAdapter.RefreshRecordWhere(AWhere);
end;

procedure TContainerDataSet<M>.Save(AObject: M);
begin
  FDataSetAdapter.Save(AObject);
end;

procedure TContainerDataSet<M>._SetAutoNextPacket(const Value: Boolean);
begin
  FDataSetAdapter.AutoNextPacket := Value;
end;

function TContainerDataSet<M>.This: TDataSetBaseAdapter<M>;
begin
  Result := FDataSetAdapter;
end;

end.

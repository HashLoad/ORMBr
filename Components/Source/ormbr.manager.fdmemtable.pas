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

unit ormbr.manager.fdmemtable;

interface

uses
  DB,
  Rtti,
  Classes,
  Generics.Collections,
  dbebr.connection.base,
  ormbr.db.manager.fdmemtable;

type
  TORMBrManagerFDMemTable = class(TComponent)
  private
    FOwner: TComponent;
    FConnection: TDBEBrConnectionBase;
    FManagerDataSet: TManagerFDMemTable;
    function GetConnection: TDBEBrConnectionBase;
    procedure SetConnection(const Value: TDBEBrConnectionBase);
    function GetOwnerNestedList: Boolean;
    procedure SetOwnerNestedList(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RemoveAdapter<T: class>;
    function AddAdapter<T: class, constructor>(const ADataSet: TDataSet;
      const APageSize: Integer = -1): TManagerFDMemTable; overload;
    function AddAdapter<T, M: class, constructor>(const ADataSet: TDataSet): TManagerFDMemTable; overload;
    function AddLookupField<T, M: class, constructor>(const AFieldName: string;
                                                      const AKeyFields: string;
                                                      const ALookupKeyFields: string;
                                                      const ALookupResultField: string;
                                                      const ADisplayLabel: string = ''): TManagerFDMemTable;
    procedure Open<T: class, constructor>; overload;
    procedure Open<T: class, constructor>(const AID: Integer); overload;
    procedure Open<T: class, constructor>(const AID: String); overload;
    procedure OpenWhere<T: class, constructor>(const AWhere: string; const AOrderBy: string = '');
    procedure Close<T: class, constructor>;
    procedure LoadLazy<T: class, constructor>(const AOwner: T);
    procedure RefreshRecord<T: class, constructor>;
    procedure EmptyDataSet<T: class, constructor>;
    procedure CancelUpdates<T: class, constructor>;
    procedure ApplyUpdates<T: class, constructor>(const MaxErros: Integer);
    procedure Save<T: class, constructor>(AObject: T);
    function Current<T: class, constructor>: T;
    function DataSet<T: class, constructor>: TDataSet;
    /// ObjectSet
    function Find<T: class, constructor>: TObjectList<T>; overload;
    function Find<T: class, constructor>(const AID: TValue): T; overload;
    function FindWhere<T: class, constructor>(const AWhere: string;
                                              const AOrderBy: string = ''): TObjectList<T>;
    function NestedList<T: class>: TObjectList<T>;
    function AutoNextPacket<T: class, constructor>(const AValue: Boolean): TManagerFDMemTable;
    property OwnerNestedList: Boolean read GetOwnerNestedList write SetOwnerNestedList;
  published
    property Connection: TDBEBrConnectionBase read GetConnection write SetConnection;
  end;

implementation

{ TDBManagerDataSet }

function TORMBrManagerFDMemTable.AddAdapter<T, M>(const ADataSet: TDataSet): TManagerFDMemTable;
begin
  Result := FManagerDataSet.AddAdapter<T, M>(ADataSet);
end;

function TORMBrManagerFDMemTable.AddAdapter<T>(const ADataSet: TDataSet;
  const APageSize: Integer): TManagerFDMemTable;
begin
  Result := FManagerDataSet.AddAdapter<T>(ADataSet, APageSize);
end;

function TORMBrManagerFDMemTable.AddLookupField<T, M>(const AFieldName, AKeyFields,
  ALookupKeyFields, ALookupResultField, ADisplayLabel: string): TManagerFDMemTable;
begin
  Result := FManagerDataSet.AddLookupField<T, M>(AFieldName, AKeyFields,
                                                 ALookupKeyFields, ALookupResultField, ADisplayLabel);
end;

procedure TORMBrManagerFDMemTable.ApplyUpdates<T>(const MaxErros: Integer);
begin
  FManagerDataSet.ApplyUpdates<T>(MaxErros);
end;

function TORMBrManagerFDMemTable.AutoNextPacket<T>(const AValue: Boolean): TManagerFDMemTable;
begin
  Result := FManagerDataSet.AutoNextPacket<T>(AValue);
end;

procedure TORMBrManagerFDMemTable.CancelUpdates<T>;
begin
  FManagerDataSet.CancelUpdates<T>;
end;

procedure TORMBrManagerFDMemTable.Close<T>;
begin
  FManagerDataSet.Close<T>;
end;

constructor TORMBrManagerFDMemTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
end;

function TORMBrManagerFDMemTable.Current<T>: T;
begin
  Result := FManagerDataSet.Current<T>;
end;

function TORMBrManagerFDMemTable.DataSet<T>: TDataSet;
begin
  Result := FManagerDataSet.DataSet<T>;
end;

destructor TORMBrManagerFDMemTable.Destroy;
begin
  if Assigned(FManagerDataSet) then
    FManagerDataSet.Free;
  inherited;
end;

procedure TORMBrManagerFDMemTable.EmptyDataSet<T>;
begin
  FManagerDataSet.EmptyDataSet<T>;
end;

function TORMBrManagerFDMemTable.Find<T>(const AID: TValue): T;
begin
  Result := FManagerDataSet.Find<T>(AID);
end;

function TORMBrManagerFDMemTable.Find<T>: TObjectList<T>;
begin
  Result := FManagerDataSet.Find<T>;
end;

function TORMBrManagerFDMemTable.FindWhere<T>(const AWhere, AOrderBy: string): TObjectList<T>;
begin
  Result := FManagerDataSet.FindWhere<T>(AWhere, AOrderBy);
end;

function TORMBrManagerFDMemTable.GetConnection: TDBEBrConnectionBase;
begin
  Result := FConnection;
end;

function TORMBrManagerFDMemTable.GetOwnerNestedList: Boolean;
begin
  Result := FManagerDataSet.OwnerNestedList;
end;

procedure TORMBrManagerFDMemTable.LoadLazy<T>(const AOwner: T);
begin
  FManagerDataSet.LoadLazy<T>(AOwner);
end;

function TORMBrManagerFDMemTable.NestedList<T>: TObjectList<T>;
begin
  Result := FManagerDataSet.NestedList<T>;
end;

procedure TORMBrManagerFDMemTable.Open<T>;
begin
  FManagerDataSet.Open<T>;
end;

procedure TORMBrManagerFDMemTable.Open<T>(const AID: Integer);
begin
  FManagerDataSet.Open<T>(AID);
end;

procedure TORMBrManagerFDMemTable.Open<T>(const AID: String);
begin
  FManagerDataSet.Open<T>(AID);
end;

procedure TORMBrManagerFDMemTable.OpenWhere<T>(const AWhere, AOrderBy: string);
begin
  FManagerDataSet.OpenWhere<T>(AWhere, AOrderBy);
end;

procedure TORMBrManagerFDMemTable.RefreshRecord<T>;
begin
  FManagerDataSet.RefreshRecord<T>;
end;

procedure TORMBrManagerFDMemTable.RemoveAdapter<T>;
begin
  FManagerDataSet.RemoveAdapter<T>;
end;

procedure TORMBrManagerFDMemTable.Save<T>(AObject: T);
begin
  FManagerDataSet.Save<T>(AObject);
end;

procedure TORMBrManagerFDMemTable.SetConnection(const Value: TDBEBrConnectionBase);
begin
  FConnection := Value;
  if Assigned(FManagerDataSet) then
    FManagerDataSet.Free;
  FManagerDataSet := TManagerFDMemTable.Create(FConnection.DBConnection);
end;

procedure TORMBrManagerFDMemTable.SetOwnerNestedList(const Value: Boolean);
begin
  FManagerDataSet.OwnerNestedList := Value;
end;

end.

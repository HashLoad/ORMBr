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

unit ormbr.manager.clientdataset;

interface

uses
  DB,
  Rtti,
  Classes,
  Generics.Collections,
  dbebr.connection.base,
  ormbr.db.manager.clientdataset;

type
  TORMBrManagerClientDataSet = class(TComponent)
  private
    FOwner: TComponent;
    FConnection: TDBEBrConnectionBase;
    FManagerDataSet: TManagerClientDataSet;
    function GetConnection: TDBEBrConnectionBase;
    procedure SetConnection(const Value: TDBEBrConnectionBase);
    function GetOwnerNestedList: Boolean;
    procedure SetOwnerNestedList(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddAdapter<T: class, constructor>(const ADataSet: TDataSet;
      const APageSize: Integer = -1): TManagerClientDataSet; overload;
    function AddAdapter<T, M: class, constructor>(const ADataSet: TDataSet): TManagerClientDataSet; overload;
    function AddLookupField<T, M: class, constructor>(const AFieldName: string;
                                                      const AKeyFields: string;
                                                      const ALookupKeyFields: string;
                                                      const ALookupResultField: string;
                                                      const ADisplayLabel: string = ''): TManagerClientDataSet;
    procedure RemoveAdapter<T: class>;
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
    function AutoNextPacket<T: class, constructor>(const AValue: Boolean): TManagerClientDataSet;
    property OwnerNestedList: Boolean read GetOwnerNestedList write SetOwnerNestedList;
  published
    property Connection: TDBEBrConnectionBase read GetConnection write SetConnection;
  end;

implementation

{ TDBManagerDataSet }

constructor TORMBrManagerClientDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
end;

destructor TORMBrManagerClientDataSet.Destroy;
begin
  if Assigned(FManagerDataSet) then
    FManagerDataSet.Free;
  inherited;
end;

function TORMBrManagerClientDataSet.AddAdapter<T, M>(const ADataSet: TDataSet): TManagerClientDataSet;
begin
  Result := FManagerDataSet.AddAdapter<T, M>(ADataSet);
end;

function TORMBrManagerClientDataSet.AddAdapter<T>(const ADataSet: TDataSet;
  const APageSize: Integer): TManagerClientDataSet;
begin
  Result := FManagerDataSet.AddAdapter<T>(ADataSet, APageSize);
end;

function TORMBrManagerClientDataSet.AddLookupField<T, M>(const AFieldName, AKeyFields,
  ALookupKeyFields, ALookupResultField, ADisplayLabel: string): TManagerClientDataSet;
begin
  Result := FManagerDataSet.AddLookupField<T, M>(AFieldName, AKeyFields,
                                                 ALookupKeyFields, ALookupResultField, ADisplayLabel);
end;

procedure TORMBrManagerClientDataSet.ApplyUpdates<T>(const MaxErros: Integer);
begin
  FManagerDataSet.ApplyUpdates<T>(MaxErros);
end;

function TORMBrManagerClientDataSet.AutoNextPacket<T>(const AValue: Boolean): TManagerClientDataSet;
begin
  Result := FManagerDataSet.AutoNextPacket<T>(AValue);
end;

procedure TORMBrManagerClientDataSet.CancelUpdates<T>;
begin
  FManagerDataSet.CancelUpdates<T>;
end;

procedure TORMBrManagerClientDataSet.Close<T>;
begin
  FManagerDataSet.Close<T>;
end;

function TORMBrManagerClientDataSet.Current<T>: T;
begin
  Result := FManagerDataSet.Current<T>;
end;

function TORMBrManagerClientDataSet.DataSet<T>: TDataSet;
begin
  Result := FManagerDataSet.DataSet<T>;
end;

procedure TORMBrManagerClientDataSet.EmptyDataSet<T>;
begin
  FManagerDataSet.EmptyDataSet<T>;
end;

function TORMBrManagerClientDataSet.Find<T>(const AID: TValue): T;
begin
  Result := FManagerDataSet.Find<T>(AID);
end;

function TORMBrManagerClientDataSet.Find<T>: TObjectList<T>;
begin
  Result := FManagerDataSet.Find<T>;
end;

function TORMBrManagerClientDataSet.FindWhere<T>(const AWhere, AOrderBy: string): TObjectList<T>;
begin
  Result := FManagerDataSet.FindWhere<T>(AWhere, AOrderBy);
end;

function TORMBrManagerClientDataSet.GetConnection: TDBEBrConnectionBase;
begin
  Result := FConnection;
end;

function TORMBrManagerClientDataSet.GetOwnerNestedList: Boolean;
begin
  Result := FManagerDataSet.OwnerNestedList;
end;

procedure TORMBrManagerClientDataSet.LoadLazy<T>(const AOwner: T);
begin
  FManagerDataSet.LoadLazy<T>(AOwner);
end;

function TORMBrManagerClientDataSet.NestedList<T>: TObjectList<T>;
begin
  Result := FManagerDataSet.NestedList<T>;
end;

procedure TORMBrManagerClientDataSet.Open<T>;
begin
  FManagerDataSet.Open<T>;
end;

procedure TORMBrManagerClientDataSet.Open<T>(const AID: Integer);
begin
  FManagerDataSet.Open<T>(AID);
end;

procedure TORMBrManagerClientDataSet.Open<T>(const AID: String);
begin
  FManagerDataSet.Open<T>(AID);
end;

procedure TORMBrManagerClientDataSet.OpenWhere<T>(const AWhere, AOrderBy: string);
begin
  FManagerDataSet.OpenWhere<T>(AWhere, AOrderBy);
end;

procedure TORMBrManagerClientDataSet.RefreshRecord<T>;
begin
  FManagerDataSet.RefreshRecord<T>;
end;

procedure TORMBrManagerClientDataSet.RemoveAdapter<T>;
begin
  FManagerDataSet.RemoveAdapter<T>;
end;

procedure TORMBrManagerClientDataSet.Save<T>(AObject: T);
begin
  FManagerDataSet.Save<T>(AObject);
end;

procedure TORMBrManagerClientDataSet.SetConnection(const Value: TDBEBrConnectionBase);
begin
  FConnection := Value;
  if Assigned(FManagerDataSet) then
    FManagerDataSet.Free;
  FManagerDataSet := TManagerClientDataSet.Create(FConnection.DBConnection);
end;

procedure TORMBrManagerClientDataSet.SetOwnerNestedList(const Value: Boolean);
begin
  FManagerDataSet.OwnerNestedList := Value;
end;

end.

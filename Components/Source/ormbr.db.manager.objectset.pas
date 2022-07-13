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
}

unit ormbr.db.manager.objectset;

interface

uses
  DB,
  Classes,
  Generics.Collections,
  dbebr.connection.base,
  ormbr.manager.objectset;

type
  TORMBrManagerObjectSet = class(TComponent)
  private
    FOwner: TComponent;
    FConnection: TDBEBrConnectionBase;
    FManagerObjectSet: TManagerObjectSet;
    function GetConnection: TDBEBrConnectionBase;
    procedure SetConnection(const Value: TDBEBrConnectionBase);
    function GetOwnerNestedList: Boolean;
    procedure SetOwnerNestedList(const Value: Boolean);
    function GetManagerObjectSet: TManagerObjectSet;
  public
    constructor Create(const AOwner: TComponent);
    destructor Destroy; override;
    function AddAdapter<T: class, constructor>(const APageSize: Integer = -1): TORMBrManagerObjectSet;
    function NestedList<T: class>: TObjectList<T>;
    /// ObjectSet
    function Find<T: class, constructor>: TObjectList<T>; overload;
    function Find<T: class, constructor>(const AID: Variant): T; overload;
    function FindWhere<T: class, constructor>(const AWhere: string;
                                              const AOrderBy: string = ''): TObjectList<T>;
    function ModifiedFields<T: class, constructor>: TDictionary<string, TDictionary<string, string>>;
    function ExistSequence<T: class, constructor>: Boolean;
    procedure LoadLazy<T: class, constructor>(const AObject: TObject); overload;
    /// <summary>
    ///   Métodos para serem usados com a propriedade OwnerNestedList := False;
    /// </summary>
    function Insert<T: class, constructor>(const AObject: T): Integer; overload;
    procedure Modify<T: class, constructor>(const AObject: T); overload;
    procedure Update<T: class, constructor>(const AObject: T); overload;
    procedure Delete<T: class, constructor>(const AObject: T); overload;
    procedure NextPacket<T: class, constructor>(var AObjectList: TObjectList<T>); overload;
    procedure New<T: class, constructor>(var AObject: T); overload;
    /// <summary>
    ///   Métodos para serem usados com a propriedade OwnerNestedList := True;
    /// </summary>
    function Current<T: class, constructor>: T; overload;
    function Current<T: class, constructor>(const AIndex: Integer): T; overload;
    function New<T: class, constructor>: Integer; overload;
   	function Insert<T: class, constructor>: Integer; overload;
    procedure Modify<T: class, constructor>; overload;
    procedure Update<T: class, constructor>; overload;
    procedure Delete<T: class, constructor>; overload;
    procedure NextPacket<T: class, constructor>; overload;
    function First<T: class, constructor>: Integer;
    function Next<T: class, constructor>: Integer;
    function Prior<T: class, constructor>: Integer;
    function Last<T: class, constructor>: Integer;
    function Bof<T: class>: Boolean;
    function Eof<T: class>: Boolean;
    property OwnerNestedList: Boolean read GetOwnerNestedList write SetOwnerNestedList;
  published
    property Connection: TDBEBrConnectionBase read GetConnection write SetConnection;
  end;

implementation

function TORMBrManagerObjectSet.AddAdapter<T>(const APageSize: Integer): TORMBrManagerObjectSet;
begin
  Result := Self;
  GetManagerObjectSet.AddAdapter<T>(APageSize);
end;

function TORMBrManagerObjectSet.Bof<T>: Boolean;
begin
  Result := GetManagerObjectSet.Bof<T>;
end;

constructor TORMBrManagerObjectSet.Create(const AOwner: TComponent);
begin
  FOwner := AOwner;
  OwnerNestedList := True;
end;

function TORMBrManagerObjectSet.Current<T>(const AIndex: Integer): T;
begin
  Result := GetManagerObjectSet.Current<T>(AIndex);
end;

function TORMBrManagerObjectSet.Current<T>: T;
begin
  Result := GetManagerObjectSet.Current<T>;
end;

procedure TORMBrManagerObjectSet.Delete<T>;
begin
  GetManagerObjectSet.Delete<T>;
end;

procedure TORMBrManagerObjectSet.Delete<T>(const AObject: T);
begin
  GetManagerObjectSet.Delete<T>(AObject);
end;

destructor TORMBrManagerObjectSet.Destroy;
begin
  if Assigned(FManagerObjectSet) then
    FManagerObjectSet.Free;
  inherited;
end;

function TORMBrManagerObjectSet.Eof<T>: Boolean;
begin
  Result := GetManagerObjectSet.Eof<T>;
end;

function TORMBrManagerObjectSet.ExistSequence<T>: Boolean;
begin
  Result := GetManagerObjectSet.ExistSequence<T>;
end;

function TORMBrManagerObjectSet.Find<T>: TObjectList<T>;
begin
  Result := GetManagerObjectSet.Find<T>;
end;

function TORMBrManagerObjectSet.Find<T>(const AID: Variant): T;
begin
  Result := GetManagerObjectSet.Find<T>(AID);
end;

function TORMBrManagerObjectSet.FindWhere<T>(const AWhere, AOrderBy: string): TObjectList<T>;
begin
  Result := GetManagerObjectSet.FindWhere<T>(AWhere, AOrderBy);
end;

function TORMBrManagerObjectSet.First<T>: Integer;
begin
  Result := GetManagerObjectSet.First<T>;
end;

function TORMBrManagerObjectSet.GetManagerObjectSet: TManagerObjectSet;
begin
  if not Assigned(FManagerObjectSet) then
    FManagerObjectSet := TManagerObjectSet.Create(FConnection.DBConnection);
  Result := FManagerObjectSet;
end;

function TORMBrManagerObjectSet.GetConnection: TDBEBrConnectionBase;
begin
  Result := FConnection;
end;

function TORMBrManagerObjectSet.GetOwnerNestedList: Boolean;
begin
  Result := GetManagerObjectSet.OwnerNestedList;
end;

function TORMBrManagerObjectSet.Insert<T>(const AObject: T): Integer;
begin
  Result := GetManagerObjectSet.Insert<T>(AObject);
end;

function TORMBrManagerObjectSet.Insert<T>: Integer;
begin
  Result := GetManagerObjectSet.Insert<T>;
end;

function TORMBrManagerObjectSet.Last<T>: Integer;
begin
  Result := GetManagerObjectSet.Last<T>;
end;

procedure TORMBrManagerObjectSet.LoadLazy<T>(const AObject: TObject);
begin
  GetManagerObjectSet.LoadLazy<T>(AObject);
end;

function TORMBrManagerObjectSet.ModifiedFields<T>: TDictionary<string, TDictionary<string, string>>;
begin
  Result := GetManagerObjectSet.ModifiedFields<T>;
end;

procedure TORMBrManagerObjectSet.Modify<T>;
begin
  GetManagerObjectSet.Modify<T>;
end;

procedure TORMBrManagerObjectSet.Modify<T>(const AObject: T);
begin
  GetManagerObjectSet.Modify<T>(AObject);
end;

function TORMBrManagerObjectSet.NestedList<T>: TObjectList<T>;
begin
  Result := GetManagerObjectSet.NestedList<T>;
end;

procedure TORMBrManagerObjectSet.New<T>(var AObject: T);
begin
  GetManagerObjectSet.New<T>(AObject);
end;

function TORMBrManagerObjectSet.New<T>: Integer;
begin
  Result := GetManagerObjectSet.New<T>;
end;

function TORMBrManagerObjectSet.Next<T>: Integer;
begin
  Result := GetManagerObjectSet.Next<T>;
end;

procedure TORMBrManagerObjectSet.NextPacket<T>(var AObjectList: TObjectList<T>);
begin
  GetManagerObjectSet.NextPacket<T>(AObjectList);
end;

procedure TORMBrManagerObjectSet.NextPacket<T>;
begin
  GetManagerObjectSet.NextPacket<T>;
end;

function TORMBrManagerObjectSet.Prior<T>: Integer;
begin
  Result := GetManagerObjectSet.Prior<T>;
end;

procedure TORMBrManagerObjectSet.SetConnection(const Value: TDBEBrConnectionBase);
begin
  FConnection := Value;
end;

procedure TORMBrManagerObjectSet.SetOwnerNestedList(const Value: Boolean);
begin
  GetManagerObjectSet.OwnerNestedList := Value;
end;

procedure TORMBrManagerObjectSet.Update<T>(const AObject: T);
begin
  GetManagerObjectSet.Update<T>(AObject);
end;

procedure TORMBrManagerObjectSet.Update<T>;
begin
  GetManagerObjectSet.Update<T>;
end;

end.

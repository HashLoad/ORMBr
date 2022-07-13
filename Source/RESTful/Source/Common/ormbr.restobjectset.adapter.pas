{
      ORM Brasil é um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.
}

{ 
  @abstract(REST Componentes)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)
  @abstract(Website : http://www.ormbr.com.br)
  @abstract(Telagram : https://t.me/ormbr)
}

unit ormbr.restobjectset.adapter;

interface

uses
  Rtti,
  TypInfo,
  Classes,
  Variants,
  SysUtils,
  Generics.Collections,
  /// ORMBr
  ormbr.objectset.base.adapter,
  ormbr.client.interfaces,
  dbcbr.mapping.classes,
  dbcbr.types.mapping,
  ormbr.objects.helper;

type
  TRESTObjectSetAdapter<M: class, constructor> = class(TObjectSetBaseAdapter<M>)
  private
    FConnection: IRESTConnection;
  protected
  public
    constructor Create(const AConnection: IRESTConnection; const APageSize: Integer = -1); overload;
    destructor Destroy; override;
    function Find: TObjectList<M>; overload; override;
    function Find(const AID: Int64): M; overload; override;
    function Find(const AID: string): M; overload; override;
    function FindWhere(const AWhere: string; const AOrderBy: string = ''): TObjectList<M>; overload; override;
    procedure Insert(const AObject: M); override;
    procedure Update(const AObject: M); override;
    procedure Delete(const AObject: M); override;
    function Find(const AMethodName: String; const AParams: array of string): TObjectList<M>; overload; override;
  end;

implementation

uses
  ormbr.session.rest,
  dbcbr.mapping.explorer,
  ormbr.core.consts;

{ TRESTObjectSetAdapter<M> }

constructor TRESTObjectSetAdapter<M>.Create(const AConnection: IRESTConnection;
  const APageSize: Integer = -1);
begin
  inherited Create;
  FConnection := AConnection;
  FSession := TSessionRest<M>.Create(AConnection, nil, APageSize);
end;

procedure TRESTObjectSetAdapter<M>.Delete(const AObject: M);
begin
  inherited;
  try
    // Executa comando delete em cascade
    CascadeActionsExecute(AObject, CascadeDelete);
    // Executa comando delete master
    FSession.Delete(AObject);
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
    end;
  end;
end;

destructor TRESTObjectSetAdapter<M>.Destroy;
begin
  FSession.Free;
  inherited;
end;

function TRESTObjectSetAdapter<M>.Find: TObjectList<M>;
begin
  inherited;
  Result := FSession.Find;
end;

function TRESTObjectSetAdapter<M>.Find(const AID: Int64): M;
begin
  inherited;
  Result := FSession.Find(AID);
end;

function TRESTObjectSetAdapter<M>.Find(const AID: string): M;
begin
  inherited;
  Result := FSession.Find(AID);
end;

function TRESTObjectSetAdapter<M>.FindWhere(const AWhere,
  AOrderBy: string): TObjectList<M>;
begin
  inherited;
  Result := FSession.FindWhere(AWhere, AOrderBy);
end;

procedure TRESTObjectSetAdapter<M>.Insert(const AObject: M);
var
  LPrimaryKey: TPrimaryKeyColumnsMapping;
  LColumn: TColumnMapping;
begin
  inherited;
  try
    FSession.Insert(AObject);
    if FSession.ExistSequence then
    begin
      LPrimaryKey := TMappingExplorer
                       .GetMappingPrimaryKeyColumns(AObject.ClassType);
      if LPrimaryKey = nil then
        raise Exception.Create(cMESSAGEPKNOTFOUND);

      for LColumn in LPrimaryKey.Columns do
        SetAutoIncValueChilds(AObject, LColumn);
    end;
  except
    on E: Exception do
    begin
      raise Exception.Create(E.Message);
    end;
  end;
end;

procedure TRESTObjectSetAdapter<M>.Update(const AObject: M);
var
  LObjectList: TObjectList<M>;
begin
  inherited;
  LObjectList := TObjectList<M>.Create;
  try
    LObjectList.Add(AObject);
    FSession.Update(LObjectList);
  finally
    LObjectList.Clear;
    LObjectList.Free;
  end;
end;

function TRESTObjectSetAdapter<M>.Find(const AMethodName: String;
  const AParams: array of string): TObjectList<M>;
begin
  inherited;
  Result := FSession.Find(AMethodName, AParams);
end;

end.

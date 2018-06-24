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

{$INCLUDE ..\ormbr.inc}

unit ormbr.restobjectset.adapter;

interface

uses
  Rtti,
  TypInfo,
  Classes,
  Variants,
  SysUtils,
  Generics.Collections,
  /// orm
  ormbr.objectset.base.adapter,
  ormbr.factory.interfaces,
  ormbr.mapping.classes,
  ormbr.types.mapping,
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
    function Find(const AID: Integer): M; overload; override;
    function Find(const AID: string): M; overload; override;
    function FindWhere(const AWhere: string; const AOrderBy: string = ''): TObjectList<M>; overload; override;
    procedure Insert(const AObject: M); override;
    procedure Update(const AObject: M); override;
    procedure Delete(const AObject: M); override;
  end;

implementation

uses
  ormbr.session.rest;

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
    /// <summary>
    /// Executa comando delete em cascade
    /// </summary>
    CascadeActionsExecute(AObject, CascadeDelete);
    /// <summary>
    /// Executa comando delete master
    /// </summary>
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

function TRESTObjectSetAdapter<M>.Find(const AID: Integer): M;
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
  LColumn: TColumnMapping;
begin
  inherited;
  try
    FSession.Insert(AObject);
    if FSession.ExistSequence then
    begin
      for LColumn in AObject.GetPrimaryKey do
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

end.

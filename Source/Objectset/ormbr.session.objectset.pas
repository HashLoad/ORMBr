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

unit ormbr.session.objectset;

interface

uses
  DB,
  Rtti,
  TypInfo,
  Classes,
  Variants,
  SysUtils,
  Generics.Collections,
  /// ORMBr
  ormbr.bind,
  ormbr.command.executor,
  ormbr.session.abstract,
  dbebr.factory.interfaces;

type
  // M - Sessão Abstract
  TSessionObjectSet<M: class, constructor> = class(TSessionAbstract<M>)
  protected
    FConnection: IDBConnection;
  public
    constructor Create(const AConnection: IDBConnection;
      const APageSize: Integer = -1); overload;
    destructor Destroy; override;
    procedure LoadLazy(const AOwner, AObject: TObject); override;
//    procedure NextPacketList(const AObjectList: TObjectList<M>); overload; override;
//    function NextPacketList: TObjectList<M>; overload; override;
//    function NextPacketList(const APageSize, APageNext: Integer): TObjectList<M>; overload; override;
//    function NextPacketList(const AWhere, AOrderBy: String;
//      const APageSize, APageNext: Integer): TObjectList<M>; overload; override;
  end;

implementation

{ TSessionObjectSet<M> }

constructor TSessionObjectSet<M>.Create(const AConnection: IDBConnection; const APageSize: Integer);
begin
  inherited Create(APageSize);
  FConnection := AConnection;
  FCommandExecutor := TSQLCommandExecutor<M>.Create(Self, AConnection, APageSize);
end;

procedure TSessionObjectSet<M>.LoadLazy(const AOwner, AObject: TObject);
begin
  inherited;
  FCommandExecutor.LoadLazy(AOwner, AObject);
end;

//function TSessionObjectSet<M>.NextPacketList(const AWhere, AOrderBy: String;
//  const APageSize, APageNext: Integer): TObjectList<M>;
//var
// LDBResultSet: IDBResultSet;
//begin
//  inherited;
//  Result := nil;
//  if FFetchingRecords then
//    Exit;
//  LDBResultSet := FCommandExecutor.NextPacketList(AWhere, AOrderBy, APageSize, APageNext);
//  Result := PopularObjectSet(LDBResultSet);
//end;

//function TSessionObjectSet<M>.NextPacketList(const APageSize, APageNext: Integer): TObjectList<M>;
//var
//  LDBResultSet: IDBResultSet;
//begin
//  inherited;
//  Result := nil;
//  if FFetchingRecords then
//    Exit;
//  LDBResultSet := FCommandExecutor.NextPacketList(APageSize, APageNext);
//  Result := PopularObjectSet(LDBResultSet);
//end;

destructor TSessionObjectSet<M>.Destroy;
begin
  FCommandExecutor.Free;
  inherited;
end;

//procedure TSessionObjectSet<M>.NextPacketList(const AObjectList: TObjectList<M>);
//begin
//  inherited;
//  if FFetchingRecords then
//    Exit;
//  FPageNext := FPageNext + FPageSize;
//  if FFindWhereUsed then
//    FCommandExecutor.NextPacketList(AObjectList, FWhere, FOrderBy, FPageSize, FPageNext)
//  else
//    FCommandExecutor.NextPacketList(AObjectList, FPageSize, FPageNext);
//
//  /// <summary>
//  ///    if AObjectList = nil then
//  ///      Exit;
//  ///    if AObjectList.RecordCount > 0 then
//  ///      Exit;
//  ///    FFetchingRecords := True;
//  ///  Esse código para definir a tag FFetchingRecords, está sendo feito no
//  ///  método NextPacketList() dentro do FCommandExecutor.
//  /// </summary>
//end;

//function TSessionObjectSet<M>.NextPacketList: TObjectList<M>;
//var
//  LDBResultSet: IDBResultSet;
//begin
//  inherited;
//  Result := nil;
//  if FFetchingRecords then
//    Exit;
//  FPageNext := FPageNext + FPageSize;
//  if FFindWhereUsed then
//    LDBResultSet := FCommandExecutor.NextPacketList(FWhere, FOrderBy, FPageSize, FPageNext)
//  else
//    LDBResultSet := FCommandExecutor.NextPacketList(FPageSize, FPageNext);
//  Result := PopularObjectSet(LDBResultSet);
//end;

end.

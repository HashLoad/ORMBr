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
  /// orm
  ormbr.objects.manager,
  ormbr.mapping.explorerstrategy,
  ormbr.session.abstract,
  ormbr.factory.interfaces;

type
  /// <summary>
  /// M - Sessão Abstract
  /// </summary>
  TSessionObjectSet<M: class, constructor> = class(TSessionAbstract<M>)
  private
  protected
    FConnection: IDBConnection;
  public
    constructor Create(const AConnection: IDBConnection; const APageSize: Integer = -1); overload;
    destructor Destroy; override;
    procedure NextPacket(const AObjectList: TObjectList<M>); override;
    procedure LoadLazy(const AOwner, AObject: TObject); override;
  end;

implementation

{ TSessionObjectSet<M> }

constructor TSessionObjectSet<M>.Create(const AConnection: IDBConnection; const APageSize: Integer);
begin
  inherited Create(APageSize);
  FConnection := AConnection;
  FManager := TObjectManager<M>.Create(Self, AConnection, APageSize);
end;

procedure TSessionObjectSet<M>.LoadLazy(const AOwner, AObject: TObject);
begin
  inherited;
  FManager.LoadLazy(AOwner, AObject);
end;

destructor TSessionObjectSet<M>.Destroy;
begin
  FManager.Free;
  inherited;
end;

procedure TSessionObjectSet<M>.NextPacket(const AObjectList: TObjectList<M>);
begin
  inherited;
  if not FManager.FetchingRecords then
    FManager.NextPacketList(AObjectList);
end;

end.

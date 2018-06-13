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

unit ormbr.container.fdmemtable;

interface

uses
  DB,
  SysUtils,
  FireDAC.Comp.Client,
  /// ormbr
  ormbr.session.dataset,
  ormbr.container.dataset,
  ormbr.factory.interfaces,
  ormbr.dataset.fdmemtable;

type
  TContainerFDMemTable<M: class, constructor> = class(TContainerDataSet<M>)
  public
    constructor Create(AConnection: IDBConnection; ADataSet: TDataSet; APageSize: Integer; AMasterObject: TObject); overload;
    constructor Create(AConnection: IDBConnection; ADataSet: TDataSet; APageSize: Integer); overload;
    constructor Create(AConnection: IDBConnection; ADataSet: TDataSet; AMasterObject: TObject); overload;
    constructor Create(AConnection: IDBConnection; ADataSet: TDataSet); overload;
    destructor Destroy; override;
  end;

implementation

{ TContainerFDMemTable }

constructor TContainerFDMemTable<M>.Create(AConnection: IDBConnection;
  ADataSet: TDataSet; APageSize: Integer; AMasterObject: TObject);
begin
  if ADataSet is TFDMemTable then
    FDataSetAdapter := TFDMemTableAdapter<M>.Create(AConnection, ADataSet, APageSize, AMasterObject)
  else
    raise Exception.Create('Is not TFDMemTable type');
end;

constructor TContainerFDMemTable<M>.Create(AConnection: IDBConnection;
  ADataSet: TDataSet; APageSize: Integer);
begin
  Create(AConnection, ADataSet, APageSize, nil);
end;

constructor TContainerFDMemTable<M>.Create(AConnection: IDBConnection;
  ADataSet: TDataSet; AMasterObject: TObject);
begin
  Create(AConnection, ADataSet, -1, AMasterObject);
end;

constructor TContainerFDMemTable<M>.Create(AConnection: IDBConnection;
  ADataSet: TDataSet);
begin
  Create(AConnection, ADataSet, -1, nil);
end;

destructor TContainerFDMemTable<M>.Destroy;
begin
  FDataSetAdapter.Free;
  inherited;
end;

end.

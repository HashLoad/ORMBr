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

unit ormbr.container.restfdmemtable;

interface

uses
  DB,
  SysUtils,
  FireDAC.Comp.Client,
  /// ormbr
  ormbr.session.dataset,
  ormbr.container.dataset,
  ormbr.factory.interfaces,
  ormbr.restdataset.fdmemtable;

type
  TContainerRESTFDMemTable<M: class, constructor> = class(TContainerDataSet<M>)
  public
    constructor Create(ADataSet: TDataSet; APageSize: Integer; AMasterObject: TObject); overload;
    constructor Create(ADataSet: TDataSet; APageSize: Integer); overload;
    constructor Create(ADataSet: TDataSet; AMasterObject: TObject); overload;
    constructor Create(ADataSet: TDataSet); overload;
    destructor Destroy; override;
  end;

implementation

{ TContainerRESTFDMemTable<M> }

constructor TContainerRESTFDMemTable<M>.Create(ADataSet: TDataSet; APageSize: Integer; AMasterObject: TObject);
begin
  if ADataSet is TFDMemTable then
    FDataSetAdapter := TRESTFDMemTableAdapter<M>.Create(ADataSet, APageSize, AMasterObject)
  else
    raise Exception.Create('Is not TFDMemTable type');
end;

constructor TContainerRESTFDMemTable<M>.Create(ADataSet: TDataSet;
  APageSize: Integer);
begin
  Create(ADataSet, APageSize, nil);
end;

constructor TContainerRESTFDMemTable<M>.Create(ADataSet: TDataSet;
  AMasterObject: TObject);
begin
  Create(ADataSet, -1, AMasterObject);
end;

constructor TContainerRESTFDMemTable<M>.Create(ADataSet: TDataSet);
begin
  Create(ADataSet, -1, nil);
end;

destructor TContainerRESTFDMemTable<M>.Destroy;
begin
  FDataSetAdapter.Free;
  inherited;
end;

end.


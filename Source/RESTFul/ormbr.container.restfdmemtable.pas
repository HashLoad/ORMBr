{
      ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{ @abstract(ORMBr Framework.)
  @created(20 Jul 2016)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @author(Skype : ispinheiro)

  ORM Brasil � um ORM simples e descomplicado para quem utiliza Delphi.
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


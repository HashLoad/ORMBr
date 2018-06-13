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

unit ormbr.container.restclientdataset;

interface

uses
  DB,
  SysUtils,
  DBClient,
  /// ormbr
  ormbr.session.dataset,
  ormbr.container.dataset,
  ormbr.factory.interfaces,
  ormbr.restdataset.clientdataset;

type
  TContainerRESTClientDataSet<M: class, constructor> = class(TContainerDataSet<M>)
  public
    constructor Create(const AConnection: IRESTConnection; ADataSet: TDataSet;
      AMasterObject: TObject); overload;
    constructor Create(const AConnection: IRESTConnection; ADataSet: TDataSet); overload;
    destructor Destroy; override;
    procedure NextPacket; override; deprecated 'Unsupported feature';
  end;

implementation

{ TContainerRESTClientDataSet<M> }

constructor TContainerRESTClientDataSet<M>.Create(const AConnection: IRESTConnection;
  ADataSet: TDataSet; AMasterObject: TObject);
begin
  if ADataSet is TClientDataSet then
    FDataSetAdapter := TRESTClientDataSetAdapter<M>.Create(AConnection, ADataSet, AMasterObject)
  else
    raise Exception.Create('Is not TClientDataSet type');
end;

constructor TContainerRESTClientDataSet<M>.Create(const AConnection: IRESTConnection;
  ADataSet: TDataSet);
begin
  Create(AConnection, ADataSet, nil);
end;

destructor TContainerRESTClientDataSet<M>.Destroy;
begin
  FDataSetAdapter.Free;
  inherited;
end;

procedure TContainerRESTClientDataSet<M>.NextPacket;
begin
  raise Exception.Create('Unsupported feature');
end;

end.

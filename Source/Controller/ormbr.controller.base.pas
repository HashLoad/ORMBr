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

unit ormbr.controller.base;

interface

uses
  ormbr.controller.abstract,
  ormbr.factory.interfaces,
  ormbr.container.objectset.interfaces,
  ormbr.container.dataset.interfaces;

type
  TControllerBase<M: class, constructor> = class(TControllerAbstract<M>)
  private
  public
    constructor Create(AConnection: IDBConnection; AContainerDataSet:
      IContainerDataSet<M>; AContainerObjectSet: IContainerObjectSet<M>); virtual;
    destructor Destroy; override;
  end;

implementation

{ TControllerBase }

constructor TControllerBase<M>.Create(AConnection: IDBConnection;
  AContainerDataSet: IContainerDataSet<M>; AContainerObjectSet: IContainerObjectSet<M>);
begin
  inherited;
end;

destructor TControllerBase<M>.Destroy;
begin
  inherited;
end;

end.

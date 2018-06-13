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

unit ormbr.monitor;

interface

uses
  DB,
  Generics.Collections;

type
  ICommandMonitor = interface
    procedure Command(ASQL: string; AParams: TParams);
  end;

  TCommandMonitor = class abstract(TInterfacedObject, ICommandMonitor)
  public
    procedure Command(ASQL: string; AParams: TParams); virtual;
  end;

implementation

{ TCommandMonitor }

procedure TCommandMonitor.Command(ASQL: string; AParams: TParams);
begin

end;

end.

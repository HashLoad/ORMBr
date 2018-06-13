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

unit ormbr.mapping.exceptions;

interface

uses
  SysUtils,
  Rtti;

type
  EClassNotRegistered = class(Exception)
  public
    constructor Create(AClass: TClass);
  end;

  EFieldNotNull = class(Exception)
  public
    constructor Create(AName: string);
  end;

  EFieldValidate = class(Exception)
  public
    constructor Create(AField: string; AMensagem: string);
  end;

  EFieldZero = class(Exception)
  public
    constructor Create(AName: string);
  end;

implementation

{ EClassNotRegistered }

constructor EClassNotRegistered.Create(AClass: TClass);
begin
   inherited CreateFmt('Classe %s n�o registrada. Registre no Initialization usando TRegisterClasses.GetInstance.RegisterClass(%s)', [AClass.ClassName]);
end;

{ EFieldNotNull }

constructor EFieldNotNull.Create(AName: string);
begin
  inherited CreateFmt('O valor da campo [ %s ] n�o pode ser Nulo!', [AName]);
end;

{ EFieldZero }

constructor EFieldZero.Create(AName: string);
begin
  inherited CreateFmt('O valor da campo [ %s ] n�o pode ser menor que zero!', [AName]);
end;

{ EFieldValidate }

constructor EFieldValidate.Create(AField: string; AMensagem: string);
begin
  inherited CreateFmt('[ %s ] %s', [AField, AMensagem]);
end;

end.

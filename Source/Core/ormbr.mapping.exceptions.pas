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
   inherited CreateFmt('Classe %s não registrada. Registre no Initialization usando TRegisterClasses.GetInstance.RegisterClass(%s)', [AClass.ClassName]);
end;

{ EFieldNotNull }

constructor EFieldNotNull.Create(AName: string);
begin
  inherited CreateFmt('O valor da campo [ %s ] não pode ser Nulo!', [AName]);
end;

{ EFieldZero }

constructor EFieldZero.Create(AName: string);
begin
  inherited CreateFmt('O valor da campo [ %s ] não pode ser menor que zero!', [AName]);
end;

{ EFieldValidate }

constructor EFieldValidate.Create(AField: string; AMensagem: string);
begin
  inherited CreateFmt('[ %s ] %s', [AField, AMensagem]);
end;

end.

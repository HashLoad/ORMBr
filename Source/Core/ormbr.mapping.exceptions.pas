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
    constructor Create(const ADisplayLabel: String);
  end;

  EFieldValidate = class(Exception)
  public
    constructor Create(const AField: string; const AMensagem: string);
  end;

  EMinimumValueConstraint = class(Exception)
  public
    constructor Create(const ADisplayLabel: String;
      const AValue: Double);
  end;

  EMaximumValueConstraint = class(Exception)
  public
    constructor Create(const ADisplayLabel: String; const AValue: Double);
  end;

  EDefaultExpression = class(Exception)
  public
    constructor Create(const ADefault, AColumnName, AClassName: string);
  end;

implementation

uses
  ormbr.mapping.attributes;

{ EClassNotRegistered }

constructor EClassNotRegistered.Create(AClass: TClass);
begin
   inherited CreateFmt('Classe %s não registrada. Registre no Initialization usando TRegisterClasses.GetInstance.RegisterClass(%s)',
                       [AClass.ClassName]);
end;

{ EFieldNotNull }

constructor EFieldNotNull.Create(const ADisplayLabel: String);
begin
  inherited CreateFmt('Campo [ %s ] não pode ser vazio',
                      [ADisplayLabel]);
end;

{ EHighestConstraint }

constructor EMinimumValueConstraint.Create(const ADisplayLabel: String;
  const AValue: Double);
begin
  inherited CreateFmt('O valor mínimo do campo [ %s ] permitido é [ %s ]!',
                      [ADisplayLabel, FloatToStr(AValue)]);
end;

{ EFieldValidate }

constructor EFieldValidate.Create(const AField: string; const AMensagem: string);
begin
  inherited CreateFmt('[ %s ] %s',
                      [AField, AMensagem]);
end;

{ EDefaultExpression }

constructor EDefaultExpression.Create(const ADefault, AColumnName, AClassName: string);
begin
  inherited CreateFmt('O valor Default [ %s ] do campo [ %s ] na classe [ %s ], é inválido!',
                      [ADefault, AColumnName, AClassName]);
end;

{ EMaximumValueConstraint }

constructor EMaximumValueConstraint.Create(const ADisplayLabel: String; const AValue: Double);
begin
  inherited CreateFmt('O valor máximo do campo [ %s ] permitido é [ %s ]!',
                      [ADisplayLabel, FloatToStr(AValue)]);
end;

end.

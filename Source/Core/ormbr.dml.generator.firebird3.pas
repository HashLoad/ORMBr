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
}

unit ormbr.dml.generator.firebird3;

interface

uses
  SysUtils,
  StrUtils,
  Rtti,
  ormbr.dml.generator.firebird,
  ormbr.driver.register,
  dbebr.factory.interfaces,
  ormbr.criteria;

type
  // Classe de banco de dados Interbase
  TDMLGeneratorFirebird3 = class(TDMLGeneratorFirebird)
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TDMLGeneratorInterbase }

constructor TDMLGeneratorFirebird3.Create;
begin
  inherited;
  FDateFormat := 'MM/dd/yyyy';
  FTimeFormat := 'HH:MM:SS';
end;

destructor TDMLGeneratorFirebird3.Destroy;
begin

  inherited;
end;

initialization
  TDriverRegister.RegisterDriver(dnFirebird3, TDMLGeneratorFirebird3.Create);

end.

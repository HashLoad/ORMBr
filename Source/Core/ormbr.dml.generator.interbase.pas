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

unit ormbr.dml.generator.interbase;

interface

uses
  SysUtils,
  StrUtils,
  Rtti,
  ormbr.dml.generator.firebird,
  ormbr.driver.register,
  ormbr.factory.interfaces,
  ormbr.criteria;

type
  /// <summary>
  /// Classe de banco de dados Interbase
  /// </summary>
  TDMLGeneratorInterbase = class(TDMLGeneratorFirebird)
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TDMLGeneratorInterbase }

constructor TDMLGeneratorInterbase.Create;
begin
  inherited;
  FDateFormat := 'MM/dd/yyyy';
  FTimeFormat := 'HH:MM:SS';
end;

destructor TDMLGeneratorInterbase.Destroy;
begin

  inherited;
end;

initialization
  TDriverRegister.RegisterDriver(dnInterbase, TDMLGeneratorInterbase.Create);

end.

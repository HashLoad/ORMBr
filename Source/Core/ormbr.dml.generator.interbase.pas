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

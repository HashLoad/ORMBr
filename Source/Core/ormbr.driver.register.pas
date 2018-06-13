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

unit ormbr.driver.register;

interface

uses
  SysUtils,
  Generics.Collections,
  ormbr.dml.interfaces,
  ormbr.types.database,
  ormbr.factory.interfaces;

type
  TDriverRegister = class
  strict private
    class var FDriver: TDictionary<TDriverName, IDMLGeneratorCommand>;
  private
    class constructor Create;
    class destructor Destroy;
  public
    class procedure RegisterDriver(const ADriverName: TDriverName;
      const ADriverSQL: IDMLGeneratorCommand);
    class function GetDriver(const ADriverName: TDriverName): IDMLGeneratorCommand;
  end;

implementation

class constructor TDriverRegister.Create;
begin
  FDriver := TDictionary<TDriverName, IDMLGeneratorCommand>.Create;
end;

class destructor TDriverRegister.Destroy;
begin
  FDriver.Clear;
  FDriver.Free;
  inherited;
end;

class function TDriverRegister.GetDriver(const ADriverName: TDriverName): IDMLGeneratorCommand;
begin
  if not FDriver.ContainsKey(ADriverName) then
    raise Exception
            .Create('O driver ' + TStrDriverName[ADriverName] + ' n�o est� registrado, adicione a unit "ormbr.dml.generator.???.pas" onde ??? nome do driver, na cl�usula USES do seu projeto!');

  Result := FDriver[ADriverName];
end;

class procedure TDriverRegister.RegisterDriver(const ADriverName: TDriverName;
  const ADriverSQL: IDMLGeneratorCommand);
begin
  FDriver.AddOrSetValue(ADriverName, ADriverSQL);
end;

end.


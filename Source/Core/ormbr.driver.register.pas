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
}

unit ormbr.driver.register;

interface

uses
  SysUtils,
  Generics.Collections,
  ormbr.dml.interfaces,
  dbebr.factory.interfaces;

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
            .Create('O driver ' + TStrDriverName[ADriverName] + ' não está registrado, adicione a unit "ormbr.dml.generator.???.pas" onde ??? nome do driver, na cláusula USES do seu projeto!');

  Result := FDriver[ADriverName];
end;

class procedure TDriverRegister.RegisterDriver(const ADriverName: TDriverName;
  const ADriverSQL: IDMLGeneratorCommand);
begin
  FDriver.AddOrSetValue(ADriverName, ADriverSQL);
end;

end.


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

unit ormbr.ddl.register;

interface

uses
  SysUtils,
  Generics.Collections,
  ormbr.ddl.interfaces,
  ormbr.types.database,
  ormbr.ddl.generator,
  ormbr.factory.interfaces;

type
  TSQLDriverRegister = class
  private
  class var
    FInstance: TSQLDriverRegister;
  private
    FDriver: TDictionary<TDriverName, IDDLGeneratorCommand>;
    constructor CreatePrivate;
  public
    { Public declarations }
    constructor Create;
    destructor Destroy; override;
    class function GetInstance: TSQLDriverRegister;
    procedure RegisterDriver(const ADriverName: TDriverName; const ADriverSQL: IDDLGeneratorCommand);
    function GetDriver(const ADriverName: TDriverName): IDDLGeneratorCommand;
  end;

implementation

constructor TSQLDriverRegister.Create;
begin
  raise Exception.Create('Para usar o SQLDriverRegister. use o método TSQLDriverRegister.GetInstance()');
end;

constructor TSQLDriverRegister.CreatePrivate;
begin
  inherited;
  FDriver := TDictionary<TDriverName, IDDLGeneratorCommand>.Create;
end;

destructor TSQLDriverRegister.Destroy;
begin
  FDriver.Free;
  inherited;
end;

function TSQLDriverRegister.GetDriver(const ADriverName: TDriverName): IDDLGeneratorCommand;
begin
  if not FDriver.ContainsKey(ADriverName) then
    raise Exception.Create('O driver ' + TStrDriverName[ADriverName] + ' não está registrado, adicione a unit "ormbr.ddl.generator.???.pas" onde ??? nome do driver, na cláusula USES do seu projeto!');

  Result := FDriver[ADriverName];
end;

procedure TSQLDriverRegister.RegisterDriver(const ADriverName: TDriverName; const ADriverSQL: IDDLGeneratorCommand);
begin
  FDriver.AddOrSetValue(ADriverName, ADriverSQL);
end;

class function TSQLDriverRegister.GetInstance: TSQLDriverRegister;
begin
   if not Assigned(FInstance) then
      FInstance := TSQLDriverRegister.CreatePrivate;

   Result := FInstance;
end;

initialization

finalization
   if Assigned(TSQLDriverRegister.FInstance) then
   begin
      TSQLDriverRegister.FInstance.Free;
   end;
end.

